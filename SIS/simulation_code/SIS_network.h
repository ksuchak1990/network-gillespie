#pragma once
#ifndef __SIS_NET_H_INCLUDED__	// Prevents class from being redefined
#define __SIS_NET_H_INCLUDED__

#include <fstream>
#include <cstdlib>
#include <numeric>
#include "graph.h"

class sis_net : public graph
{
protected:
	// 1. DATA MEMBERS

	// Model parameters
	double valb, valgam;

	// 2. ACCESS METHODS

	// Update propensities
	std::vector< std::vector<double> > update_propensity()
	{
		// Set up
		int N = get_size();
		int M = get_number_react();
        std::list<int>::iterator listiter;
		std::vector< std::vector<double> > props(N, std::vector<double>(M));

		// Loop through rows
		for (int i = 0; i < N; i++)
		{
			// Birth propensities
			if (get_state(i) == 0)
			{
		        std::list<int> k = get_neighbours(i);

    		    for (listiter = k.begin(); listiter != k.end(); listiter++)
	    	    {
					if (get_state(*listiter) == 1)
					{
						props[i][0] += valb;
					}
    	    	}
			}
			// Death propensities
			if (get_state(i) == 1)
			{
				props[i][1] = valgam;
			}
		}

		return props;
	}

	// Update system
	void update_state(int reaction)
	{
		// Birth
		if (reaction < get_size())
		{
			if (get_state(reaction) == 0)
			{
				set_state(reaction,1);
			}
			// Can't have birth if node state is already I
			else
			{
				std::cout << "Error: invalid reaction" << std::endl;
			}
		}
		// Death
		else
		{
			if (get_state(reaction - get_size()) == 1)
			{
				set_state(reaction - get_size(),0);
			}
			// Can't have death if node state is already S
			else
			{
				std::cout << "Error: invalid reaction" << std::endl;
			}
		}
	}

	// Aggregate number of infected
	int get_I()
	{
		std::vector<int> vec = get_all_states();
		int total = std::accumulate(vec.begin(),vec.end(),0);
        return total;
	}

public:

	// 1. CONSTRUCTOR METHODS

	// Constructor given number of nodes
	sis_net(double b, double gam, int V) : graph(V)
	{
		valb = b;
		valgam = gam;
	}

	// Constructor given graph
	sis_net(double b, double gam, graph g) : graph(g)
	{
		valb = b;
		valgam = gam;
	}

	// Copy constructor
    sis_net(const sis_net& m) : graph(m.get_adj_list(), m.get_all_states())
    {
    	valb = m.get_b();
    	valgam = m.get_gam();
    }

	// 2. ACCESS METHODS

    // Access b
    const double& get_b() const
    {
    	return valb;
    }

    // Access gamma
    const double& get_gam() const
    {
    	return valgam;
    }

    // Access number of reactions
	int get_number_react()
	{
		int a = 2;
		return a;
	}

};

#endif	// SIS_NET_H

