#include <math.h>
#include <stdio.h>

enum class Measure {
  Length,
  Mass,
  Time,
  Angle,
  Charge,
  Temperature,
  Luminosity,
  Amount
};

constexpr int totalMeasures = 8;

template <Measure M> struct MeasureExponent final {
  static constexpr Measure getMeasure() { return M; }

  const int exp;

  explicit inline constexpr MeasureExponent(const int exp) : exp(exp) {}

  inline constexpr auto operator+(const MeasureExponent &m) const {
    return MeasureExponent(exp + m.exp);
  }

  inline constexpr auto operator-(const MeasureExponent &m) const {
    return MeasureExponent(exp - m.exp);
  }
};

/*
constexpr const char *const MeasureNames[totalMeasures] = {
    "Length", "Mass",        "Time",       "Angle",
    "Charge", "Temperature", "Luminosity", "Amount"};

constexpr const char *const MeasureUnits[totalMeasures] = {
    "Meter",   "Kilogram", "Second",  "Radian",
    "Coulomb", "Kelvin",   "Candela", "Mole"};
*/

constexpr const char *const MeasureUnitSymbols[totalMeasures] = {
    "m", "kg", "s", "rad", "C", "K", "cd", "mol"};

using NumericalType = double;

using LengthExponent = MeasureExponent<Measure::Length>;
using MassExponent = MeasureExponent<Measure::Mass>;
using TimeExponent = MeasureExponent<Measure::Time>;
using AngleExponent = MeasureExponent<Measure::Angle>;
using ChargeExponent = MeasureExponent<Measure::Charge>;
using TemperatureExponent = MeasureExponent<Measure::Temperature>;
using LuminosityExponent = MeasureExponent<Measure::Luminosity>;
using AmountExponent = MeasureExponent<Measure::Amount>;

template <LengthExponent Le, MassExponent Ma, TimeExponent Ti, AngleExponent An,
          ChargeExponent Ch, TemperatureExponent Te, LuminosityExponent Lu,
          AmountExponent Am>
class Quantity final {
private:
  static inline char units[256];
  static inline bool unitsPrinted = false;

public:
  static constexpr LengthExponent getLengthExponent() { return Le; }

  static constexpr MassExponent getMassExponent() { return Ma; }

  static constexpr TimeExponent getTimeExponent() { return Ti; }

  static constexpr AngleExponent getAngleExponent() { return An; }

  static constexpr ChargeExponent getChargeExponent() { return Ch; }

  static constexpr TemperatureExponent getTemperatureExponent() { return Te; }

  static constexpr LuminosityExponent getLuminosityExponent() { return Lu; }

  static constexpr AmountExponent getAmountExponent() { return Am; }

  inline constexpr auto &operator+=(const Quantity &rhs) {
    value += rhs.value;
    return *this;
  }

  inline constexpr auto &operator-=(const Quantity &rhs) {
    value -= rhs.value;
    return *this;
  }

  inline constexpr auto operator+(const Quantity &rhs) {
    return Quantity<Le, Ma, Ti, An, Ch, Te, Lu, Am>(value + rhs.value);
  }

  inline constexpr auto operator-(const Quantity &rhs) {
    return Quantity<Le, Ma, Ti, An, Ch, Te, Lu, Am>(value - rhs.value);
  }

  template <class Right> inline constexpr auto operator*(const Right &rhs) {
    using Left = Quantity;

    constexpr LengthExponent le(Left::getLengthExponent() +
                                Right::getLengthExponent());
    constexpr MassExponent ma(Left::getMassExponent() +
                              Right::getMassExponent());
    constexpr TimeExponent ti(Left::getTimeExponent() +
                              Right::getTimeExponent());
    constexpr AngleExponent an(Left::getAngleExponent() +
                               Right::getAngleExponent());
    constexpr ChargeExponent ch(Left::getChargeExponent() +
                                Right::getChargeExponent());
    constexpr TemperatureExponent te(Left::getTemperatureExponent() +
                                     Right::getTemperatureExponent());
    constexpr LuminosityExponent lu(Left::getLuminosityExponent() +
                                    Right::getLuminosityExponent());
    constexpr AmountExponent am(Left::getAmountExponent() +
                                Right::getAmountExponent());

    return Quantity<le, ma, ti, an, ch, te, lu, am>(value * rhs.value);
  }

  template <class Right> inline constexpr auto operator/(const Right &rhs) {
    using Left = Quantity;

    constexpr LengthExponent le(Left::getLengthExponent() -
                                Right::getLengthExponent());
    constexpr MassExponent ma(Left::getMassExponent() -
                              Right::getMassExponent());
    constexpr TimeExponent ti(Left::getTimeExponent() -
                              Right::getTimeExponent());
    constexpr AngleExponent an(Left::getAngleExponent() -
                               Right::getAngleExponent());
    constexpr ChargeExponent ch(Left::getChargeExponent() -
                                Right::getChargeExponent());
    constexpr TemperatureExponent te(Left::getTemperatureExponent() -
                                     Right::getTemperatureExponent());
    constexpr LuminosityExponent lu(Left::getLuminosityExponent() -
                                    Right::getLuminosityExponent());
    constexpr AmountExponent am(Left::getAmountExponent() -
                                Right::getAmountExponent());

    return Quantity<le, ma, ti, an, ch, te, lu, am>(value / rhs.value);
  }

  explicit inline constexpr Quantity(const NumericalType value,
                                     const char *const variable = nullptr)
      : value(value), variable(variable) {}

  void setVariableString(const char *const string) { this->variable = string; }

private:
  template <class T> static void setUnit(const T &exponent, int &count) {
    constexpr const char *const onefmt = "%s ";
    constexpr const char *const pwrfmt = "%s^%d ";

    const int exp = exponent.exp;
    constexpr int measureIndex = static_cast<int>(T::getMeasure());

    if (exp != 0) {
      const char *symbol = MeasureUnitSymbols[measureIndex];
      if (exp == 1) {
        count += snprintf(units + count, sizeof(units) - count, onefmt, symbol);
      } else {
        count +=
            snprintf(units + count, sizeof(units) - count, pwrfmt, symbol, exp);
      }
    }
  }

  static void setUnits() {
    int count = 0;

    setUnit(Le, count);
    setUnit(Ma, count);
    setUnit(Ti, count);
    setUnit(An, count);
    setUnit(Ch, count);
    setUnit(Te, count);
    setUnit(Lu, count);
    setUnit(Am, count);

    if (count > 0) {
      units[count - 1] = '\0';
    }

    unitsPrinted = true;
  }

public:
  static const char *getUnits() {
    if (!unitsPrinted) {
      setUnits();
    }

    return units;
  }

  NumericalType value;
  const char *variable;
};

///////////////////////////////////////////////////////////////////////////////
//
// Base Quantities
//
///////////////////////////////////////////////////////////////////////////////
using DimensionlessQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using LengthQuantity =
    Quantity<LengthExponent(1), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using MassQuantity =
    Quantity<LengthExponent(0), MassExponent(1), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using TimeQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(1),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AngleQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(1), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using ChargeQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(1), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using TemperatureQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(1),
             LuminosityExponent(0), AmountExponent(0)>;
using LuminosityQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(1), AmountExponent(0)>;
using AmountQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(1)>;

///////////////////////////////////////////////////////////////////////////////
//
// Derived Quantities
//
///////////////////////////////////////////////////////////////////////////////
using SpeedQuantity =
    Quantity<LengthExponent(1), MassExponent(0), TimeExponent(-1),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AccelerationQuantity =
    Quantity<LengthExponent(1), MassExponent(0), TimeExponent(-2),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using ForceQuantity =
    Quantity<LengthExponent(1), MassExponent(1), TimeExponent(-2),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using MomentumQuantity =
    Quantity<LengthExponent(1), MassExponent(1), TimeExponent(-1),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using EnergyQuantity =
    Quantity<LengthExponent(2), MassExponent(1), TimeExponent(-2),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using FrequencyQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(-1),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AngularFrequencyQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(-1),
             AngleExponent(1), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AngularAccelerationQuantity =
    Quantity<LengthExponent(0), MassExponent(0), TimeExponent(-2),
             AngleExponent(1), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using MassMomentOfInertiaQuantity =
    Quantity<LengthExponent(2), MassExponent(1), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using TorqueQuantity =
    Quantity<LengthExponent(2), MassExponent(1), TimeExponent(-2),
             AngleExponent(1), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AngularMomentumQuantity =
    Quantity<LengthExponent(2), MassExponent(1), TimeExponent(-1),
             AngleExponent(1), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using AreaQuantity =
    Quantity<LengthExponent(2), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using VolumeQuantity =
    Quantity<LengthExponent(3), MassExponent(0), TimeExponent(0),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;
using PressureQuantity =
    Quantity<LengthExponent(-1), MassExponent(1), TimeExponent(-2),
             AngleExponent(0), ChargeExponent(0), TemperatureExponent(0),
             LuminosityExponent(0), AmountExponent(0)>;

template <class T> inline void print(const T &quantity) {
  if (quantity.variable) {
    printf("%s = %.2lf [%s]\n", quantity.variable, quantity.value,
           quantity.getUnits());
  } else {
    printf("%.2lf [%s]\n", quantity.value, quantity.getUnits());
  }
}

template <int Numerator, int Denominator = 1, class T = DimensionlessQuantity>
inline constexpr auto getRationalPower(const T &quantity) {
  static_assert(Denominator != 0);

  constexpr int iLe = T::getLengthExponent().exp;
  constexpr int iMa = T::getMassExponent().exp;
  constexpr int iTi = T::getTimeExponent().exp;
  constexpr int iAn = T::getAngleExponent().exp;
  constexpr int iCh = T::getChargeExponent().exp;
  constexpr int iTe = T::getTemperatureExponent().exp;
  constexpr int iLu = T::getLuminosityExponent().exp;
  constexpr int iAm = T::getAmountExponent().exp;

  constexpr int iLeNum = iLe * Numerator;
  constexpr int iMaNum = iMa * Numerator;
  constexpr int iTiNum = iTi * Numerator;
  constexpr int iAnNum = iAn * Numerator;
  constexpr int iChNum = iCh * Numerator;
  constexpr int iTeNum = iTe * Numerator;
  constexpr int iLuNum = iLu * Numerator;
  constexpr int iAmNum = iAm * Numerator;

  static_assert(iLeNum % Denominator == 0);
  static_assert(iMaNum % Denominator == 0);
  static_assert(iTiNum % Denominator == 0);
  static_assert(iAnNum % Denominator == 0);
  static_assert(iChNum % Denominator == 0);
  static_assert(iTeNum % Denominator == 0);
  static_assert(iLuNum % Denominator == 0);
  static_assert(iAmNum % Denominator == 0);

  constexpr LengthExponent le(iLeNum / Denominator);
  constexpr MassExponent ma(iMaNum / Denominator);
  constexpr TimeExponent ti(iTiNum / Denominator);
  constexpr AngleExponent an(iAnNum / Denominator);
  constexpr ChargeExponent ch(iChNum / Denominator);
  constexpr TemperatureExponent te(iTeNum / Denominator);
  constexpr LuminosityExponent lu(iLuNum / Denominator);
  constexpr AmountExponent am(iAmNum / Denominator);

  constexpr auto numerator = static_cast<NumericalType>(Numerator);
  constexpr auto denominator = static_cast<NumericalType>(Denominator);
  constexpr auto power = numerator / denominator;

  return Quantity<le, ma, ti, an, ch, te, lu, am>(pow(quantity.value, power));
}

template <class T, class U>
inline constexpr auto getIrrationalPower(const T &quantity, const U &power) {

  static_assert(T::getLengthExponent() == 0);
  static_assert(T::getMassExponent() == 0);
  static_assert(T::getTimeExponent() == 0);
  static_assert(T::getAngleExponent() == 0);
  static_assert(T::getChargeExponent() == 0);
  static_assert(T::getTemperatureExponent() == 0);
  static_assert(T::getLuminosityExponent() == 0);
  static_assert(T::getAmountExponent() == 0);

  static_assert(U::getLengthExponent() == 0);
  static_assert(U::getMassExponent() == 0);
  static_assert(U::getTimeExponent() == 0);
  static_assert(U::getAngleExponent() == 0);
  static_assert(U::getChargeExponent() == 0);
  static_assert(U::getTemperatureExponent() == 0);
  static_assert(U::getLuminosityExponent() == 0);
  static_assert(U::getAmountExponent() == 0);

  return DimensionlessQuantity(pow(quantity.value, power.value));
}

template <class T> inline constexpr auto getSquare(const T &quantity) {
  return getRationalPower<2>(quantity);
}

template <class T> inline constexpr auto getSquareRoot(const T &quantity) {
  return getRationalPower<1, 2>(quantity);
}

template <class T> inline constexpr auto getCube(const T &quantity) {
  return getRationalPower<3>(quantity);
}

template <class T> inline constexpr auto getCubeRoot(const T &quantity) {
  return getRationalPower<1, 3>(quantity);
}

int main() {
  LengthQuantity x(150.00, "x");
  TimeQuantity t(5.00, "t");

  // Speed v
  auto v = x / t;
  v.setVariableString("v");

  print(v);

  DimensionlessQuantity half(0.50);
  MassQuantity m(5.00, "m");

  // Kinetic energy K
  auto K = half * m * v * v;
  K.setVariableString("K");

  print(K);

  LengthQuantity X(1.67);
  LengthQuantity Y(-3.45);
  LengthQuantity Z(-8.21);

  auto magnitude = getSquareRoot(getSquare(X) + getSquare(Y) + getSquare(Z));
  magnitude.setVariableString("|r|");

  print(magnitude);
}
