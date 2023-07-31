#include <cstdint>
#include <iostream>
#include <type_traits>

using UInt64 = std::uint64_t;

namespace Measure {

enum class Type {
  Length,
  Mass,
  Time,
  Angle,
  Charge,
  Temperature,
  Luminosity,
  Amount
};

enum class Sign { Positive, Negative };

template <UInt64 Minuend, UInt64 Subtrahend>
[[nodiscard]] consteval auto getAbsoluteDifference() -> UInt64 {
  return (Subtrahend < Minuend) ? (Minuend - Subtrahend)
                                : (Subtrahend - Minuend);
}

template <UInt64 Numerator, UInt64 Denominator>
[[nodiscard]] consteval auto getGreatestCommonFactor() -> UInt64 {
  if constexpr (Denominator) {
    return getGreatestCommonFactor<Denominator, Numerator % Denominator>();
  } else {
    return Numerator;
  }
}

template <UInt64 Numerator, UInt64 Denominator>
[[nodiscard]] consteval auto getReducedNumerator() -> UInt64 {
  return Numerator / getGreatestCommonFactor<Numerator, Denominator>();
}

template <UInt64 Numerator, UInt64 Denominator>
[[nodiscard]] consteval auto getReducedDenominator() -> UInt64 {
  return Denominator / getGreatestCommonFactor<Numerator, Denominator>();
}

template <Type Measure, Sign Sense, UInt64 Numerator, UInt64 Denominator>
struct [[nodiscard]] RationalExponent final {
  using ReducedExponent =
      RationalExponent<Measure, Sense,
                       getReducedNumerator<Numerator, Denominator>(),
                       getReducedDenominator<Numerator, Denominator>()>;

  static_assert(std::is_same_v<RationalExponent, ReducedExponent>);

  [[nodiscard]] static consteval auto getMeasure() -> Type { return Measure; }
  [[nodiscard]] static consteval auto getSense() -> Sign { return Sense; }
  [[nodiscard]] static consteval auto getNumerator() -> UInt64 {
    return Numerator;
  }
  [[nodiscard]] static consteval auto getDenominator() -> UInt64 {
    return Denominator;
  }

  template <UInt64 OtherNumerator, UInt64 OtherDenominator>
  using AddedExponent = RationalExponent<
      Measure, Numerator * OtherDenominator + OtherNumerator * Denominator,
      Denominator * OtherDenominator>;

  template <UInt64 OtherNumerator, UInt64 OtherDenominator>
  using SubtractedExponent = RationalExponent<
      Measure, Numerator * OtherDenominator - OtherNumerator * Denominator,
      Denominator * OtherDenominator>;
};

/*
template <Type Measure, UInt64 Numerator = Unit, UInt64 Denominator = Unit>
struct RationalExponent final {
  static consteval auto getMeasure() { return Measure; }
  static consteval auto getNumerator() { return Numerator; }
  static consteval auto getDenominator() { return Denominator; }

  using ThisType = RationalExponent;

  template <UInt64 OtherNumerator, UInt64 OtherDenominator>
  using ReducedExponent = RationalExponent<
      Measure, getReducedNumerator<OtherNumerator, OtherDenominator>(),
      getReducedDenominator<OtherNumerator, OtherDenominator>()>;

  template <UInt64 OtherNumerator, UInt64 OtherDenominator>
  using AddedExponent = RationalExponent<
      Measure, Numerator * OtherDenominator + OtherNumerator * Denominator,
      Denominator * OtherDenominator>;

  template <UInt64 OtherNumerator, UInt64 OtherDenominator>
  using SubtractedExponent = RationalExponent<
      Measure, Numerator * OtherDenominator - OtherNumerator * Denominator,
      Denominator * OtherDenominator>;
};

using LengthExponent = RationalExponent<Type::Length>;
using MassExponent = RationalExponent<Type::Mass>;
using TimeExponent = RationalExponent<Type::Time>;
using AngleExponent = RationalExponent<Type::Angle>;
using ChargeExponent = RationalExponent<Type::Charge>;
using TemperatureExponent = RationalExponent<Type::Temperature>;
using LuminosityExponent = RationalExponent<Type::Luminosity>;
using AmountExponent = RationalExponent<Type::Amount>;

template <LengthExponent Le, MassExponent Ma, TimeExponent Ti, AngleExponent An,
          ChargeExponent Ch, TemperatureExponent Te, LuminosityExponent Lu,
          AmountExponent Am>
class PhysicalQuantity final {
public:
};
*/

} // namespace Measure

int main() {
  constexpr auto x = Measure::RationalExponent<Measure::Type::Length,
                                               Measure::Sign::Positive, 1, 6>();
}
