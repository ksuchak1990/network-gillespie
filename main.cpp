#include "system.h"
#include <fstream>

using namespace std;

int main()
{
    ofstream outfile("output.txt");
    srand((unsigned int)time(NULL));

    int N = 250;          // Network size
    double b = 0.5, l=0.05;     // Model parameter
    int a = 2;          // System mode
                        // 1: SI network
                        // 2: SIS network

    for (int i = 0; i < 100; i++)
    {
        // Set up graph initial state
        graph g(N);
        g.make_complete();
        g.set_state(0);
        // g.set_state(0,1);
        for (int j = 0; j < 5; j++)
        {
            g.set_state(i+j,1);
        }

        // Make SI network model
        sis_net m(b,l,g);

        // Make time-dependent system based on model 
        mysystem s(m);

        // Run Gillespie algorithm for system
        s.Gillespie(outfile, a);
    }

    // for (int i = 0; i < 100; i++)
    // {
    //     // Set up graph initial state
    //     graph g(N);
    //     g.make_complete();
    //     g.set_state(0);
    //     for (int j = 0; j < 5; j++)
    //     {
    //         g.set_state(j,1);
    //     }
    
    //     // Make SI/SIS network model
    //     si_net m(b,g);
    //     sis_net n(b,gam,g);

    //     // Make time-dependent system based on model 
    //     mysystem s(m);
    //     mysystem t(n);


    //     // Run Gillespie algorithm for system
    //     t.Gillespie(outfile);
    // }

    return 0;
}
