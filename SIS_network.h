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

	double valb, valgam;

	// 2. ACCESS METHODS

	// Update propensities
	std::vector< std::vector<double> > update_propensity()
	{
		int N = get_size();
		int M = get_number_react();
        std::list<int>::iterator listiter;
		std::vector< std::vector<double> > props(N, std::vector<double>(M));
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
						props[i][0] += valb;
					}
    	    	}
			}
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
		else if (reaction <= get_number_react() * get_size() && reaction > get_number_react())
		{
			if (get_state(reaction - get_size()) == 1)
			{
				set_state(reaction - get_size(),0);
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
	sis_net(double b, double gam, int V) : graph(V)
	{
		valb = b;
		valgam = gam;
	}

	sis_net(double b, double gam, graph g) : graph(g)
	{
		valb = b;
		valgam = gam;
	}

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

    const double& get_gam() const
    {
    	return valgam;
    }

	int get_number_react()
	{
		int a = 2;
		return a;
	}



};

#endif	// SIS_NET_H

