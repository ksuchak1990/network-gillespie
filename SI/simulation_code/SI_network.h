#pragma once
#ifndef __SI_NET_H_INCLUDED__	// Prevents class from being redefined
#define __SI_NET_H_INCLUDED__

#include <fstream>
#include <cstdlib>
#include <numeric>
#include "graph.h"

class si_net : public graph
{
protected:
	// 1. DATA MEMBERS

	double valb;

	// 2. ACCESS METHODS

	// Update propensities
	std::vector< std::vector<double> > update_propensity()
	{
		int N = get_size();
		int M = get_number_react();
        std::list<int>::iterator listiter;
		std::vector< std::vector<double> > props(N, std::vector<double>(M));
		for (int j = 0 ; j < M; j++)
		{
			for (int i = 0; i < N; i++)
			{
				if (get_state(i) == 0)
				{
			        std::list<int> k = get_neighbours(i);
	    		    std::list<int> mylist;
	    		    int x;

	    		    for (listiter = k.begin(); listiter != k.end(); listiter++)
		    	    {
						if (get_state(*listiter) == 1)
						{
							props[i][j] += valb;
						}
	    	    	}
				}
			}

		}
		return props;
	}

	// Update system
	void update_state(int reaction)
	{
		if (reaction <= get_size())
		{
			if (get_state(reaction) == 0)
			{
				set_state(reaction,1);
			}
			else
			{
				std::cout << "Error: invalid reaction" << std::endl;
			}
		}
		else
		{
			std::cout << "Error: reaction outside vertex range" << std::endl;
		}
	}

	// Aggregate number of infected
	int get_I()
	{
		std::vector<int> vec = get_all_states();
		int total = std::accumulate(vec.begin(),vec.end(),0);
        return total;
	}

	// // Print state to screen
	// void state()
	// {
	// 	std::cout << valt << '\t' << get_I() << std::endl;
	// }

	// // Print state to file
	// void state(std::ofstream& fout)
	// {
	// 	fout << valt << '\t' << get_I() << std::endl;
	// }

public:

	// 1. CONSTRUCTOR METHODS

	// Constructor for 
	si_net(double b, int V) : graph(V)
	{
		valb = b;
	}

	si_net(double b, graph g) : graph(g)
	{
		valb = b;
	}

    si_net(const si_net& m) : graph(m.get_adj_list(), m.get_all_states())
    {
    	valb = m.get_b();
    }

	// 2. ACCESS METHODS

    // Access b
    const double& get_b() const
    {
    	return valb;
    }

	int get_number_react()
	{
		int a = 1;
		return a;
	}



};

#endif	// SI_NET_H

