#include "system.h"

using namespace std;

int main()
{
    ofstream outfile("output.txt");
    int N = 250;
    double b = 0.5;

    for (int i = 0; i < 10; i++)
    {
        graph g(N);
        g.make_complete();
        g.set_state(0);
        for (int j = 0; j < 5; j++)
        {
            g.set_state(i+j,1);
        }

        mysystem s(b,g);

        s.Gillespie(outfile);
    }




    // vector<int> z = g.get_all_states();
    // vector< list<int> > y = g.get_adj_list();

    // graph f(g);

    // int k = s.get_state(0);
    // int l = f.get_state(0);

    // g.set_state(0,0);

    // graph h(g);

    // int m = h.get_state(0);


    // cout << "State of vertex 0 in g is:" << '\t' << k << endl;
    // cout << "State of vertex 0 in f is:" << '\t' << l << endl;
    // cout << "State of vertex 0 in h is:" << '\t' << m << endl;


    // system si(0.5, 3);


    return 0;
}
