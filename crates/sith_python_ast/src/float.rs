use core::fmt;
use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
    str::FromStr,
};

/// Note: This type diverges from `f64` which doesn't implement Eq
/// because NaNs are considered equal to themselves in our implementation.
#[derive(Clone, Copy)]
pub struct Float(FloatInner);

#[derive(Clone, Copy)]
enum FloatInner {
    FloatNumber { upper: u32, lower: u32 },
    NaN,
}

impl FromStr for Float {
    type Err = std::num::ParseFloatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Float::from_f64(s.parse::<f64>()?))
    }
}

impl Float {
    pub fn nan() -> Self {
        Self(FloatInner::NaN)
    }

    pub fn from_f64(f: f64) -> Self {
        if f.is_nan() {
            return Self::nan();
        }

        // Get the 64-bit IEEE 754 representation as u64
        let bits = f.to_bits();
        // Extract lower 32 bits
        let lower = bits as u32;
        // Extract upper 32 bits
        let upper = (bits >> 32) as u32;
        Float(FloatInner::FloatNumber { upper, lower })
    }

    pub fn to_f64(&self) -> f64 {
        match self.0 {
            FloatInner::FloatNumber { upper, lower } => {
                // Combine into u64
                let bits = ((upper as u64) << 32) | (lower as u64);
                f64::from_bits(bits)
            }
            FloatInner::NaN => f64::NAN,
        }
    }

    pub fn is_zero(&self) -> bool {
        let FloatInner::FloatNumber { upper, lower } = self.0 else {
            return false;
        };

        // In IEEE 754, zero is represented with all exponent and mantissa bits set to 0
        // The sign bit can be either 0 (positive zero) or 1 (negative zero)
        // So we check if the lower 63 bits are all zero (ignoring the sign bit)
        (upper & 0x7FFFFFFF) == 0 && lower == 0
    }
}

impl std::fmt::Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_f64())
    }
}

impl Debug for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            FloatInner::FloatNumber { upper, lower } => {
                write!(f, "Float({:#x}, {:#x}) [{}]", upper, lower, self.to_f64())
            }
            FloatInner::NaN => write!(f, "Float(NaN)"),
        }
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        if self.is_zero() && other.is_zero() {
            return true;
        }
        match (self.0, other.0) {
            (
                FloatInner::FloatNumber { upper, lower },
                FloatInner::FloatNumber {
                    upper: other_upper,
                    lower: other_lower,
                },
            ) => upper == other_upper && lower == other_lower,
            (FloatInner::FloatNumber { .. }, FloatInner::NaN)
            | (FloatInner::NaN, FloatInner::FloatNumber { .. }) => false,
            (FloatInner::NaN, FloatInner::NaN) => true,
        }
    }
}
impl Eq for Float {}

impl Hash for Float {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.is_zero() {
            // Use the same hash value for both +0.0 and -0.0
            return 0u64.hash(state);
        }
        match self.0 {
            FloatInner::FloatNumber { upper, lower } => {
                upper.hash(state);
                lower.hash(state);
            }
            FloatInner::NaN => {
                // Use a specific value for NaN
                0xFFFFFFFF_FFFFFFFFu64.hash(state);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Float;

    #[test]
    fn test_float_display() {
        let float = Float::from_f64(3.145153);
        assert_eq!(format!("{float}"), String::from("3.145153"))
    }

    fn assert_f64_bits_eq(expected: f64, actual: f64, msg: &str) {
        assert_eq!(
            expected.to_bits(),
            actual.to_bits(),
            "{}: expected {:?} (bits: {:x}), got {:?} (bits: {:x})",
            msg,
            expected,
            expected.to_bits(),
            actual,
            actual.to_bits()
        );
    }

    #[test]
    fn test_conversion_positive_zero() {
        let original = 0.0;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Positive zero conversion failed");
    }

    #[test]
    fn test_conversion_negative_zero() {
        let original = -0.0;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Negative zero conversion failed");
    }

    #[test]
    fn test_conversion_one() {
        let original = 1.0;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "1.0 conversion failed");
    }

    #[test]
    fn test_conversion_negative_one() {
        let original = -1.0;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "-1.0 conversion failed");
    }

    #[test]
    fn test_conversion_pi() {
        let original = std::f64::consts::PI;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Pi conversion failed");
    }

    #[test]
    fn test_conversion_infinity() {
        let original = f64::INFINITY;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Infinity conversion failed");
    }

    #[test]
    fn test_conversion_negative_infinity() {
        let original = f64::NEG_INFINITY;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Negative infinity conversion failed");
    }

    #[test]
    fn test_conversion_nan() {
        let original = f64::NAN;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert!(
            converted.is_nan(),
            "NaN conversion failed: expected NaN, got {:?}",
            converted
        );
        // Note: We don't compare bits directly since NaN can have multiple representations
    }

    #[test]
    fn test_conversion_subnormal() {
        let original = f64::MIN_POSITIVE / 2.0; // A subnormal number
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Subnormal conversion failed");
    }

    #[test]
    fn test_conversion_max_value() {
        let original = f64::MAX;
        let float_struct = Float::from_f64(original);
        let converted = float_struct.to_f64();
        assert_f64_bits_eq(original, converted, "Max value conversion failed");
    }

    #[test]
    fn test_eq_positive_numbers() {
        let float1 = Float::from_f64(1.25);
        let float2 = Float::from_f64(1.25);
        #[allow(clippy::approx_constant)]
        let float3 = Float::from_f64(3.14);
        assert_eq!(float1, float2, "1.25 should equal 1.25");
        assert_ne!(float1, float3, "1.25 should not equal 3.14");
    }

    #[test]
    fn test_eq_zeros() {
        let pos_zero = Float::from_f64(0.0);
        let neg_zero = Float::from_f64(-0.0);
        assert_eq!(
            pos_zero, neg_zero,
            "Positive and negative zero should be equal"
        );
    }

    #[test]
    fn test_eq_infinity() {
        let inf1 = Float::from_f64(f64::INFINITY);
        let inf2 = Float::from_f64(f64::INFINITY);
        let neg_inf = Float::from_f64(f64::NEG_INFINITY);
        assert_eq!(inf1, inf2, "Infinity should equal infinity");
        assert_ne!(inf1, neg_inf, "Infinity should not equal negative infinity");
    }

    #[test]
    fn test_eq_nan() {
        let nan = Float::nan();
        assert_eq!(nan, nan, "NaN should be equal to NaN");
    }

    #[test]
    fn test_eq_subnormal() {
        let sub1 = Float::from_f64(f64::MIN_POSITIVE / 2.0);
        let sub2 = Float::from_f64(f64::MIN_POSITIVE / 2.0);
        let normal = Float::from_f64(f64::MIN_POSITIVE);
        assert_eq!(sub1, sub2, "Subnormals should be equal");
        assert_ne!(sub1, normal, "Subnormal should not equal normal");
    }
}
