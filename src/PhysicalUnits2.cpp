#include <cstdint>

using Integer = std::int64_t;

namespace Measure {

template <Integer Measure, Integer Numerator, Integer Denominator>
struct RationalExponent final {
  static consteval auto getMeasure() { return Measure; }
  static consteval auto getNumerator() { return Numerator; }
  static consteval auto getDenominator() { return Denominator; }
};

template <class... RationalExponentTypes> class PhysicalQuantity final {
public:
};

} // namespace Measure
