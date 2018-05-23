#include "graph.h"
#include <string>
#include <sstream>

using namespace std;

std::vector<int> read_state(string);
std::vector< std::list<int> > read_adj(string);

int main()
{
    //*** CONSTRUCTION OF GRAPH AND PRINTING TO FILE ***//

    ofstream outfile1("./adj.txt");
    ofstream outfile2("./state.txt");
    int N = 5, I_init = 0;

    // Set up graph initial state
    graph g(N);

    // Built-in graphs
    // g.make_complete();
    // g.make_star();
    // g.make_line();

    // Custom graph
    g.add_edge(0,2);
    g.add_edge(1,2);
    g.add_edge(2,3);
    g.add_edge(2,4);
    g.add_edge(3,4);

    // Set states
    g.set_state(0);
    for (int j = 0; j < I_init; j++)
    {
        g.set_state((j)%N,1);
    }

    // print adjacency list and state vector to respective files
    g.print(outfile1,outfile2);


    //*** READ FROM FILE AND CONSTRUCT GRAPH ***//

    // Addresses of state and adjacency list to read
    string in1 = "./state.txt";
    string in2 = "./adj.txt";

    std::vector<int> myst = read_state(in1);
    std::vector< list<int> > adj = read_adj(in2);

    // Construct graph based on output files
    graph h(adj,myst);


    //*** CHECK NEW GRAPH AGAINST ORIGINAL GRAPH ***//

    N = g.get_size();
    int M = h.get_size();
    int z = 0;
    list<int> neigh_g, neigh_h;

    // Check graph sizes
    if (N == M)
    {
        cout << "check 1 complete: graphs are same size" << endl;
    }
    else if (N != M)
    {
        cout << "check 1 failed: graphs are different size" << endl;
    }

    // Check graph states
    for (int i = 0; i < N; ++i)
    {
        if (g.get_state(i) == h.get_state(i))
        {
            z++;
        }
    }
    if (z == N)
    {
        cout << "check 2 complete: graphs have all the same states" << endl;
    }
    else if (z != 100)
    {
        cout << "check 2 failed: graphs have different states" << endl;
    }

    z = 0;

    // Check adjacency lists
    for (int i = 0; i < N; ++i)
    {
        neigh_g = g.get_neighbours(i);
        neigh_h = h.get_neighbours(i);
        if (neigh_g == neigh_h)
        {
            z++;
        }

    }
    if (z == N)
    {
        cout << "check 3 complete: graphs have same adjacency matrices" << endl;
    }
    else if (z != 100)
    {
        cout << "check 3 failed: graphs have different adjacency matrices" << endl;
    }

    return 0;
}

// Read in state vector from file
std::vector<int> read_state(string in)
{
    ifstream inputfile1(in);    // Read in states
    std::vector<int> myst;

    if(inputfile1)
    {
        int val;
        while(inputfile1 >> val)
        {
            myst.push_back(val);    // Add each value from file to end
        }
    }
    return myst;

}

// Read in adjacency list from file
std::vector< std::list<int> > read_adj(string in)
{
    ifstream inputfile2(in);        // Read in adjacency list
    std::vector< std::list<int> > adj;

    int n, count(1);

    if (inputfile2.is_open())
    {
        // Line: list for current node
        for (string line; getline(inputfile2, line); )
        {
            adj.resize(count);
            stringstream stream(line);
            while(stream >> n)
            {
                adj[count-1].push_back(n);  // Add value to end for this node
            }
            count++;
        }

    }
    return adj;

}
