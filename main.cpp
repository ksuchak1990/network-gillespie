#include "system.h"

using namespace std;

int main()
{

    int N = 3;
    double b = 0.5;

    graph g(N);

    mysystem s(b,N);
    mysystem t(b,g);


    s.set_state(1);

    // vector<int> z = g.get_all_states();
    // vector< list<int> > y = g.get_adj_list();

    // graph f(g);

    int k = s.get_state(0);
    // int l = f.get_state(0);

    // g.set_state(0,0);

    // graph h(g);

    // int m = h.get_state(0);


    cout << "State of vertex 0 in g is:" << '\t' << k << endl;
    // cout << "State of vertex 0 in f is:" << '\t' << l << endl;
    // cout << "State of vertex 0 in h is:" << '\t' << m << endl;


    // system si(0.5, 3);


    return 0;
}
