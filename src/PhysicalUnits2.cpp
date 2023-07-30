#include <cstdint>
#include <iostream>

using Integer = std::int64_t;

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

constexpr Integer Unit = 1;

template <Integer Numerator, Integer Denominator>
consteval auto getGreatestCommonFactor() {
  static_assert(Numerator >= Integer{});
  static_assert(Denominator >= Integer{});

  if constexpr (Denominator == Integer{}) {
    return Numerator;
  } else {
    return getGreatestCommonFactor<Denominator, Numerator % Denominator>();
  }
}

template <Integer Numerator, Integer Denominator>
consteval auto getReducedNumerator() {
  static_assert(Numerator >= Integer{});
  static_assert(Denominator >= Integer{});

  return Numerator / getGreatestCommonFactor<Numerator, Denominator>();
}

template <Integer Numerator, Integer Denominator>
consteval auto getReducedDenominator() {
  static_assert(Numerator >= Integer{});
  static_assert(Denominator >= Integer{});

  return Denominator / getGreatestCommonFactor<Numerator, Denominator>();
}

/*
template <Type Measure, Integer Numerator = Unit, Integer Denominator = Unit>
struct RationalExponent final {
  static consteval auto getMeasure() { return Measure; }
  static consteval auto getNumerator() { return Numerator; }
  static consteval auto getDenominator() { return Denominator; }

  using ThisType = RationalExponent;

  template <Integer OtherNumerator, Integer OtherDenominator>
  using ReducedExponent = RationalExponent<
      Measure, getReducedNumerator<OtherNumerator, OtherDenominator>(),
      getReducedDenominator<OtherNumerator, OtherDenominator>()>;

  template <Integer OtherNumerator, Integer OtherDenominator>
  using AddedExponent = RationalExponent<
      Measure, Numerator * OtherDenominator + OtherNumerator * Denominator,
      Denominator * OtherDenominator>;

  template <Integer OtherNumerator, Integer OtherDenominator>
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
  constexpr auto numerator = Measure::getReducedNumerator<21, 74>();
  constexpr auto denominator = Measure::getReducedDenominator<21, 74>();
  std::cout << numerator << "/" << denominator << std::endl;
}
