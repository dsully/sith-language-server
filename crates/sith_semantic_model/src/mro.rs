use std::collections::VecDeque;

use rustc_hash::FxHashSet;

use crate::{
    db::SymbolTableDb,
    type_inference::{ClassBase, ClassType, KnownClass},
};

// TODO: return a proper error type instead of a string
pub fn compute_mro(db: &SymbolTableDb, class: ClassType) -> Result<Vec<ClassBase>, String> {
    let class_bases = class.class_bases(db);

    if !class_bases.is_empty() && class.is_cyclically_defined(db) {
        return Err("cyclically defined class".into());
    }

    match class_bases.as_slice() {
        // `builtins.object` is the special case:
        // the only class in Python that has an MRO with length <2
        [] if class.is_object() => Ok(vec![ClassBase::Class(class)]),

        // All other classes in Python have an MRO with length >=2.
        // Even if a class has no explicit base classes,
        // it will implicitly inherit from `object` at runtime;
        // `object` will appear in the class's `__bases__` list and `__mro__`:
        //
        // ```pycon
        // >>> class Foo: ...
        // ...
        // >>> Foo.__bases__
        // (<class 'object'>,)
        // >>> Foo.__mro__
        // (<class '__main__.Foo'>, <class 'object'>)
        // ```
        [] => Ok(vec![
            ClassBase::Class(class),
            ClassBase::Class(KnownClass::object(db)),
        ]),
        // Fast path for a class that has only a single explicit base.
        //
        // This *could* theoretically be handled by the final branch below,
        // but it's a common case (i.e., worth optimizing for),
        // and the `c3_merge` function requires lots of allocations.
        [single_base] => ClassBase::try_from_type(single_base).map_or_else(
            || Err("invalid bases".into()),
            |single_base| match single_base {
                ClassBase::Unknown => {
                    Ok(
                        [ClassBase::Unknown, ClassBase::Class(KnownClass::object(db))]
                            .into_iter()
                            .collect(),
                    )
                }
                ClassBase::Class(single_class) => {
                    compute_mro(db, single_class).map(|class_bases| {
                        std::iter::once(ClassBase::Class(class))
                            .chain(class_bases)
                            .collect()
                    })
                }
            },
        ),
        // The class has multiple explicit bases.
        //
        // We'll fallback to a full implementation of the C3-merge algorithm to determine
        // what MRO Python will give this class at runtime
        // (if an MRO is indeed resolvable at all!)
        multiple_bases => {
            let mut valid_bases = vec![];
            let mut invalid_bases = vec![];

            for (i, base) in multiple_bases.iter().enumerate() {
                match ClassBase::try_from_type(base) {
                    Some(valid_base) => valid_bases.push(valid_base),
                    None => invalid_bases.push((i, base)),
                }
            }

            if !invalid_bases.is_empty() {
                return Err("invalid bases".into());
            }

            let mut seqs = vec![VecDeque::from([ClassBase::Class(class)])];
            for base in &valid_bases {
                match base {
                    ClassBase::Unknown => seqs.push(
                        [ClassBase::Unknown, ClassBase::Class(KnownClass::object(db))]
                            .into_iter()
                            .collect(),
                    ),
                    ClassBase::Class(class) => {
                        seqs.push(compute_mro(db, *class)?.into());
                    }
                }
            }
            seqs.push(valid_bases.iter().copied().collect());

            c3_merge(seqs).ok_or_else(|| {
                let mut seen_bases = FxHashSet::default();
                let mut duplicate_bases = vec![];
                for (index, base) in valid_bases
                    .iter()
                    .enumerate()
                    .filter_map(|(index, base)| Some((index, base.into_class()?)))
                {
                    if !seen_bases.insert(base) {
                        duplicate_bases.push((index, base));
                    }
                }

                if duplicate_bases.is_empty() {
                    "unresolvable MRO".into()
                    // MroErrorKind::UnresolvableMro {
                    //     bases_list: valid_bases.into_boxed_slice(),
                    // }
                } else {
                    "duplicated bases".into()
                    // MroErrorKind::DuplicateBases(duplicate_bases.into_boxed_slice())
                }
            })
        }
    }
}

fn c3_merge(mut sequences: Vec<VecDeque<ClassBase>>) -> Option<Vec<ClassBase>> {
    // Most MROs aren't that long...
    let mut mro = Vec::with_capacity(8);

    loop {
        sequences.retain(|sequence| !sequence.is_empty());

        if sequences.is_empty() {
            return Some(mro);
        }

        // If the candidate exists "deeper down" in the inheritance hierarchy,
        // we should refrain from adding it to the MRO for now. Add the first candidate
        // for which this does not hold true. If this holds true for all candidates,
        // return `None`; it will be impossible to find a consistent MRO for the class
        // with the given bases.
        let mro_entry = sequences.iter().find_map(|outer_sequence| {
            let candidate = outer_sequence[0];

            let not_head = sequences
                .iter()
                .all(|sequence| sequence.iter().skip(1).all(|base| base != &candidate));

            not_head.then_some(candidate)
        })?;

        mro.push(mro_entry);

        // Make sure we don't try to add the candidate to the MRO twice:
        for sequence in &mut sequences {
            if sequence[0] == mro_entry {
                sequence.pop_front();
            }
        }
    }
}
