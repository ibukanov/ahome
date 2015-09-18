#include <math.h>
#include <assert.h>

#include "xfunctions.h"
#include "datastruct.h"
#include "assert.h"

const double rad_per_degree = M_PI / 180.0;  

inline double pow2(double x) 
{
    return x * x;
}

ConstData::ConstData(ResourceSource& r)
{
    electron_mass =  1e-3 * getDouble(r, "electron_mass");

    alpha_em = 1.0 / getDouble(r,"alpha_em_inverce");
    double alpha_4PI = alpha_em * M_4_PI;
    e_charge = sqrt(alpha_4PI);

    Z0_mass  = getDouble(r, "Z0_mass");
    Z0_mass_in2 = in2(Z0_mass); 
    Z0_width = getDouble(r, "Z0_width");
    W_mass   = getDouble(r, "W_mass");
    W_mass_in2 = in2(W_mass);
    W_width  = getDouble(r, "W_width");

    sin_in2_thetaW = getDouble(r, "sin_in2_thetaW");
    cos_in2_thetaW = 1 - sin_in2_thetaW;
    cos_2thetaW = cos_in2_thetaW - sin_in2_thetaW;
    cos_thetaW = sqrt(cos_in2_thetaW);
    tan_in2_thetaW = sin_in2_thetaW / cos_in2_thetaW; 

    g_week_in2 = alpha_4PI / sin_in2_thetaW;
    g_week = sqrt(g_week_in2);

    cross_normalization = getDouble(r, "cross_normalization");

    e = e_charge;
    double tmp = - g_week / (4 * cos_thetaW);
    aZ = tmp * (2 * cos_2thetaW - 1);
    bZ = tmp;

}

Intervals::Intervals(ResourceSource& r)
 : mu_re(getInterval(r, "mu_re")),
   mu_im(getInterval(r, "mu_im")),
   tan_beta(getInterval(r, "tan_beta")),
   Lambda_tilda_mass(getInterval(r, "Lambda_tilda_mass")),
   sneutrinoMass(getInterval(r, "scalar_neutrino_mass")),
   sneutrinoWidth(getInterval(r, "scalar_neutrino_width")),
   mu_as_x_and_y(getBool(r, "mu_as_x_and_y"))
{
}

BeamParams::BeamParams(ResourceSource& r)
 : com_energy(getInterval(r, "com_energy"))
{
    total_cross_section = getBool(r, "total_cross_section");
    if (!total_cross_section) {
        //scatering_angle.read(r, "scatering_angle");
        //scatering_angle.begin *= rad_per_degree;
        //scatering_angle.end *= rad_per_degree;
    } 
}

void CSet::calculate(
    double mu_re, double mu_im, double tan_beta, double Lambda_tilda_mass,
    const ConstData& c) 
{
  
    double cos_in2_beta = 1 / (1 + in2(tan_beta));
    double sin_in2_beta = in2(tan_beta) / (1 + in2(tan_beta));
    double cos_2beta = (1 - in2(tan_beta)) / (1 + in2(tan_beta));
    double sin_2beta = 2 * tan_beta / (1 + in2(tan_beta));

    double abs_mu_in2 = in2(mu_re) + in2(mu_im);

    double S = (in2(Lambda_tilda_mass) + abs_mu_in2) / 2  + c.W_mass_in2;
    double T = in2(in2(Lambda_tilda_mass) - abs_mu_in2) / 4
               + in2(c.W_mass_in2 * cos_2beta)
               + c.W_mass_in2 
                 * (in2(Lambda_tilda_mass) + abs_mu_in2 
                    + 2 * Lambda_tilda_mass * mu_re * sin_2beta);

    chargino_mass_in2 = S - sqrt(T);

    double eta_X 
     = (in2(Lambda_tilda_mass) - abs_mu_in2 + 2 * c.W_mass_in2 * cos_2beta)
       / (SQRT_8 * c.W_mass)
       / sqrt(in2(Lambda_tilda_mass) * sin_in2_beta 
              + abs_mu_in2 * cos_in2_beta 
              + mu_re * Lambda_tilda_mass * sin_2beta);

    double eta_Y
     = (in2(Lambda_tilda_mass) - abs_mu_in2 - 2 * c.W_mass_in2 * cos_2beta)
       / (SQRT_8 * c.W_mass)
       / sqrt(in2(Lambda_tilda_mass) * cos_in2_beta 
              + abs_mu_in2 * sin_in2_beta 
              + mu_re * Lambda_tilda_mass * sin_2beta);

    double tan_thetaX = sqrt(1 + in2(eta_X)) - eta_X;
    double tan_thetaY = sqrt(1 + in2(eta_Y)) - eta_Y;

    cos_in2_thetaX = 1 / (1 + in2(tan_thetaX));
    cos_in2_thetaY = 1 / (1 + in2(tan_thetaY));


    AZ = c.g_week / (4 * c.cos_thetaW) 
                   * (2 * c.cos_2thetaW + cos_in2_thetaX + cos_in2_thetaY);

    BZ = - c.g_week / (4 * c.cos_thetaW) 
                   * (cos_in2_thetaX - cos_in2_thetaY);
  
}   


void CharginoCrossSections::calculate(
  double comEnergy, double sneutrinoMass, double sneutrinoWidth, 
  const ConstData& c, const CSet& p) {

  clear();

  double s = in2(comEnergy), s_in2 = in2(s);
  double m_in2 = p.chargino_mass_in2;

  if (s < 4 * m_in2) {
    return;
  }
  double rho = m_in2 / s;
  double sqrt1 = sqrt(1 - 4*rho);

  double xZ_re = c.Z0_mass_in2;
  double xZ_im = -c.Z0_mass * c.Z0_width;
  double abs_xZ_in2 = in2(xZ_re)  + in2(xZ_im);

// Calculation of the boson-boson diagrams
  
  double ZPropogators_abs_in2 = 1 / (s_in2 + abs_xZ_in2 - 2 * xZ_re * s);
  
  gamma_gamma = in4(c.e) / (M_4_PI * s) * sqrt1 * (1 + 2*rho) / 3;

  Z_Z = (in2(c.aZ) + in2(c.bZ)) / M_4_PI 
      * ZPropogators_abs_in2 * s * sqrt1 
      *  (in2(p.AZ)*(1 + 2*rho) + in2(p.BZ)*(1 - 4*rho)) / 3;

  gamma_Z = -(in2(c.e) * c.aZ * p.AZ) / M_4_PI 
      * ZPropogators_abs_in2 * (s - xZ_re)
      * sqrt1 * (1 + 2*rho) / 3;

  assert(gamma_gamma + Z_Z + 2 * gamma_Z >= 0);

// Calculation of the sneitrino-sneutrino diagrams
  
  double x_re = in2(sneutrinoMass);
  double x_im = -sneutrinoMass * sneutrinoWidth;
  double abs_x_in2 = in2(x_re)  + in2(x_im);

  double t_plus = s*(2*rho - 1 + sqrt1)/2;
  double t_minus = s*(2*rho - 1 - sqrt1)/2;

  double log_abs = 
    log((in2(t_plus) + abs_x_in2  - 2 * t_plus * x_re) 
        / (in2(t_minus) + abs_x_in2  - 2 * t_minus * x_re)) / 2;

  double phi = atan2(s*sqrt1 * x_im, 
                     in2(m_in2) + abs_x_in2 + s*(1 - 2*rho)*x_re);
  
  double re_x_r = x_re + s*(1-rho); 
  double re_x_r_in2 = in2(re_x_r);
  double re2 = re_x_r_in2 - in2(x_im);
  double im2 = 2*re_x_r*x_im;

  Nu_Nu = in2(c.g_week_in2 * p.cos_in2_thetaX) / (64 * M_PI * s_in2)
        *(re2 / x_im * phi + 2*re_x_r*log_abs + s*sqrt1);

             

// Calculation of the boson-sneutrino diagrams

// First I do with gamma-sneitrino

  gamma_Nu = -in2(c.e)*c.g_week_in2*p.cos_in2_thetaX / (32 * M_PI * s_in2 * s)
        * ((re2 + s_in2*rho)*log_abs 
           - x_im * re_x_r * phi
           + s * sqrt1 * (x_re - s*rho + s*1.5));
  assert(Nu_Nu + 2*gamma_Nu + gamma_gamma >= 0);
         
// Now lets do with Z-sneitrino

  {
    double z_re = ZPropogators_abs_in2 * (s - xZ_re);
    double z_im = -ZPropogators_abs_in2 * xZ_im;

    double f1 = ((z_re*re2 - z_im*im2) * log_abs
                - (z_re*im2 + z_im*re2) * phi) / s_in2
                + sqrt1*(z_re*x_re/s - z_im*x_im/s + z_re*(1.5 - rho));

    double f2 = rho*(z_re*log_abs - z_im*phi);

    Z_Nu = in2(c.g_week_in2) * c.tan_in2_thetaW * p.cos_in2_thetaX 
                / (64*M_PI)
              * (f1*(c.cos_2thetaW + p.cos_in2_thetaY)
                 + f2*(c.cos_2thetaW + p.cos_in2_thetaX));
  }          
  assert(Nu_Nu + 2*Z_Nu + Z_Z >= 0);

  total = gamma_gamma + Z_Z + 2 * gamma_Z + Nu_Nu + 2*gamma_Nu + 2*Z_Nu;
  assert(total >= 0.0);

}


