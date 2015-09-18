#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/param.h>

#include "xfunctions.h"
#include "resources.h"

#include "datastruct.h"

struct DataSeparator 
{
    FILE* output;
    const ClosedInterval* mostInternal;
    DataSeparator(FILE* _output): output(_output), mostInternal(NULL) { }
    void make(const ClosedInterval& i) 
    {
        if (i.pointCount() > 1) 
        {
            if (mostInternal == NULL) { mostInternal = &i; }
            fprintf(output, (mostInternal == &i) ? "\n" : "\n\n###########\n");
        }
    }
};


inline FILE *openOutDataFile(ResourceSource& res, const char *resName)
{
    char *name = getString(res, resName);
    FILE *file = xfopen(name, "wt");
    delete[] name;
    return file;
}

void calculate_cs(
    const char* charsForMass,
    const Intervals& intervals, const ConstData& constData, const CSet& set,
    const BeamParams& beamParam, 
    DataSeparator& separator, FILE* crossSectionOut) 
{
    for (unsigned i = 0; i < intervals.sneutrinoMass.pointCount(); ++i) 
    {
        double sneutrinoMass = intervals.sneutrinoMass.point(i);
        for (unsigned j = 0; j < intervals.sneutrinoWidth.pointCount(); ++j) 
        {
            double sneutrinoWidth = intervals.sneutrinoWidth.point(j);
            for (unsigned k = 0; k < beamParam.com_energy.pointCount(); ++k) 
            {
                double energy = beamParam.com_energy.point(k);
                CharginoCrossSections ccs;
                ccs.calculate(energy, sneutrinoMass, sneutrinoWidth, 
                              constData, set);
                double norm = constData.cross_normalization;
                fprintf(crossSectionOut, 
                        "%s % .6e % .6e % .6e % .6e"
                        " % .6e % .6e % .6e % .6e % .6e\n",
                        charsForMass,
                        energy,
                        ccs.total*norm, 
                        ccs.gamma_gamma*norm, ccs.Z_Z*norm, ccs.Nu_Nu*norm, 
                        ccs.gamma_Z*norm, ccs.gamma_Nu*norm, ccs.Z_Nu*norm,
                        sneutrinoMass);
            }
            separator.make(beamParam.com_energy); 
        }
        separator.make(intervals.sneutrinoWidth); 
    }    
    separator.make(intervals.sneutrinoMass); 
}
  
void loopMassMuTan(
    ConstData& constData, const Intervals& intervals,
    const BeamParams& beamParam,
    DataSeparator& massSeparator, DataSeparator& csSeparator,
    FILE* massOut, FILE* crossSectionOut) 
{
    for (unsigned i = 0; i < intervals.Lambda_tilda_mass.pointCount(); ++i) 
    {
        double lambdaMass = intervals.Lambda_tilda_mass.point(i);
        for (unsigned j_re = 0; j_re < intervals.mu_re.pointCount(); ++j_re) 
        {
            double mu1 = intervals.mu_re.point(j_re);
            for (unsigned j_im = 0; j_im < intervals.mu_im.pointCount(); ++j_im) 
            {
                double mu2 = intervals.mu_im.point(j_im);
                double mu_re, mu_im;
                if (intervals.mu_as_x_and_y) 
                {
                    mu_re = mu1;
                    mu_im = mu2;
                }
                else
                {
                    mu_re = mu1 * cos(mu2);
                    mu_im = mu1 * sin(mu2);
                }
                for (unsigned k = 0; k < intervals.tan_beta.pointCount(); ++k) 
                { 
                    double tan_beta = intervals.tan_beta.point(k);

                    CSet set;
                    set.calculate(mu_re, mu_im, tan_beta, lambdaMass, 
                                  constData);
                    char charsForMass[100];
                    sprintf(charsForMass, " % .6e % .6e % .6e % .6e", 
                            lambdaMass, mu1, mu2, tan_beta); 
                    double mass = ysqrt(set.chargino_mass_in2);
                    fprintf(massOut, "%s % .6e\n", charsForMass, mass);
                    calculate_cs(charsForMass, 
                                 intervals, constData, set, beamParam, 
                                 csSeparator, crossSectionOut);
                }
                csSeparator.make(intervals.tan_beta); 
                massSeparator.make(intervals.tan_beta); 
            }
            csSeparator.make(intervals.mu_im); 
            massSeparator.make(intervals.mu_im); 
        }      
        csSeparator.make(intervals.mu_re); 
        massSeparator.make(intervals.mu_re); 
    }
    csSeparator.make(intervals.Lambda_tilda_mass); 
    massSeparator.make(intervals.Lambda_tilda_mass); 
}

void xexit(int exitCode) 
{ 
    if (exitCode != 0) 
    {
        fprintf(stderr, "The program is terminated with exit code %d.\n", 
                exitCode);
    }            
    exit(exitCode); 
}


void printFileHeaders(FILE* massOut, FILE* crossSectionOut) 
{
    char titleMass[100];
    sprintf(titleMass, "#%12s %13s %13s %13s", 
            "Lambda-mass", "mu-re", "mu-im", "tan-beta");
    fprintf(massOut, "%s %13s\n", titleMass, "mass");        
    fprintf(crossSectionOut, 
            "%s %13s %13s %13s %13s %13s %13s %13s %13s %13s\n", titleMass,
            "energy", "cs-total", "gamma-gamma", "Z-Z", "nu-nu",
            "gamma-Z", "gamma-nu", "Z-nu", "nuMass");        
}

int main (int argc, char *argv[]) 
{
    if (!preprocessCommands(&argc, argv, NULL, NULL)) {
    	xexit(0); 
	}

    printf("Resource Reading...\n");
    ResourceSource* res = setupParameters(true, &argc, argv);

    ConstData constData(*res);
    Intervals intervals(*res);
    BeamParams beamParams(*res);

    FILE* massOut = openOutDataFile(*res, "mass_out");
    FILE* crossSectionOut = openOutDataFile(*res, "total_cross_section_out");
    printFileHeaders(massOut, crossSectionOut);

    printf("Main loop...\n");
    DataSeparator massSeparator(massOut), csSeparator(crossSectionOut);
    loopMassMuTan(constData, intervals, beamParams, massSeparator, csSeparator,
                  massOut, crossSectionOut);
    printf("Resource releasing...\n");

    fclose(crossSectionOut);
    fclose(massOut);

    delete res;

    printf("Done...\n");
    xexit(0);

}





