#ifndef _DATASTRUCT_H_
#define _DATASTRUCT_H_

#include "resources.h"
#include "math.h"

typedef int boolean;

#define true  1
#define false 0

struct ConstData 
{
  ConstData(ResourceSource& r); 
   
  double alpha_em;
  double e_charge; // e_charge = sqrt(alpha_em * 4 * PI);
  
  double electron_mass; 

  double Z0_mass;  
  double Z0_width; 
  double Z0_mass_in2;

  double W_mass, W_mass_in2;   
  double W_width;  
   
  double cos_thetaW, sin_in2_thetaW, cos_in2_thetaW, cos_2thetaW, 
         tan_in2_thetaW;
  double g_week, g_week_in2;  /* g_week_2 = alpha_em / sin_thetaW_2 */
   
  double cross_normalization;
   
  double e;
  double aZ, bZ;
};

struct Intervals
{
  Intervals(ResourceSource& r);
  
  ClosedInterval mu_re;
  ClosedInterval mu_im;
  ClosedInterval tan_beta;
  ClosedInterval Lambda_tilda_mass;
  ClosedInterval sneutrinoMass;
  ClosedInterval sneutrinoWidth;
  bool mu_as_x_and_y;
};

struct CSet 
{
    double chargino_mass_in2;
    double cos_in2_thetaX;
    double cos_in2_thetaY;
    double AZ, BZ;

    void calculate(
        double mu_re, double mu_im, double tan_beta, double Lambda_tilda_mass, 
        const ConstData&);
};

struct BeamParams 
{
  
  BeamParams(ResourceSource&);
  
  ClosedInterval com_energy;
  bool total_cross_section;
  ClosedInterval scatering_angle;
};

struct CharginoCrossSections 
{
    double gamma_gamma;
    double Z_Z;
    double Nu_Nu;
    double gamma_Z;
    double gamma_Nu;
    double Z_Nu;

    double total;


    void calculate(
        double comEnergy, double sneutrinoMass, double sneutrinoWidth, 
        const ConstData&, const CSet&);

    void clear() 
    {
        gamma_gamma = 0.0;
        Z_Z = 0.0;
        Nu_Nu = 0.0;
        gamma_Z = 0.0;
        gamma_Nu = 0.0;
        Z_Nu = 0.0;
        total = 0.0;
    } 

};


const double SQRT_2 = sqrt(2.0);
const double SQRT_8 = sqrt(8.0);
const double M_4_PI = 4 * M_PI;

inline double in2(double x) { return x * x; }
inline double in3(double x) { return x * x * x; }
inline double in4(double x) { return in2(in2(x)); }

inline double ysqrt(double x, const char* file, int line)
{
    if (x < 0.0) 
    {
        if (x > -1e-15) 
        {
            fprintf(
                stderr, "ysqrt(x) -> 0.0 in %s at line %d, x: %g\n", 
                file, line, x);
            return 0;
        }
        fprintf(
            stderr, "ysqrt(x): 0 > x in %s at line %d, x: %g\n", 
            file, line, x);
        xexit(1);
    }
    return sqrt(x);
}

#define ysqrt(x) ysqrt(x, __FILE__, __LINE__)


#endif











