#include "system.h"
#include <fstream>
#include <string>

using namespace std;

int main(int argc, char* argv[])
{
    if (argc != 5)
    {
        cout << "ERROR - SIS network program requires 3 arguments:" << endl;
        cout << "1) Network size, N (integer)" << endl;
        cout << "2) Transmission rate, b (double)" << endl;
        cout << "3) Recovery rate, l (double)" << endl;
        cout << "4) Intial number of infected, I_init (integer)" << endl;
        return 0;
    }

    int N = stoi(argv[1]);          // Network size
    double b = stod(argv[2]);       // Transmission rate
    double l = stod(argv[3]);       // Recovery rate
    int I_init = stoi(argv[4]);     // Initial number of infected

    // cout << N << '\t' << b << '\t' << l << '\t' << I_init << endl;

    ofstream outfile("./output/output.txt");
    srand((unsigned int)time(NULL));

    char o = 's';       // Output type
                        // a: aggregate
                        // s: all states

    char r = 'm';       // Run until:
                        // a: absorbing state
                        // m: max number of iterations

    for (int i = 0; i < 2500; i++)
    {
        // Set up graph initial state
        graph g(N);

        // g.make_complete();
        // g.make_star();

        // Custom graph
        g.add_edge(0,2);
        g.add_edge(1,2);
        g.add_edge(2,3);
        g.add_edge(2,4);
        g.add_edge(3,4);

        g.set_state(0);
        for (int j = 0; j < I_init; j++)
        {
            g.set_state((i+j)%N,1);
        }

        // Make SIS network model
        sis_net m(b,l,g);

        // Make time-dependent system based on model 
        mysystem s(m);

        // Run Gillespie algorithm for system
        s.Gillespie(outfile,o,r);
    }
    
    return 0;
}
