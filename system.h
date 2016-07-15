#pragma once
#ifndef __SYSTEM_H_INCLUDED__	// Prevents class from being redefined
#define __SYSTEM_H_INCLUDED__

#include <fstream>
#include <cstdlib>
#include <cmath>
#include <numeric>
#include "graph.h"

class mysystem : public graph
{
private:
	// 1. DATA MEMBERS

	double valb, valt;

	// 2. ACCESS METHODS

	// Random number - 0<r<1
	double get_rn01()
	{
		return ((double)rand() / (RAND_MAX));
	}

	// Get time until next change
	double get_tau(double r, double alpha0)
	{
		return (1/alpha0) * log(1/r)  ;
	}

	// Update propensities
	std::vector<double> update_propensity()
	{
		int N = get_size();
        std::list<int>::iterator listiter;
		std::vector<double> props(N);
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
						props[i] += valb;
					}
    	    	}
			}
		}
		return props;
	}

	void update_state(double tau,int reaction)
	{
		valt += tau;
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

	double get_alpha0()
	{

	}

	int get_I()
	{
		std::vector<int> vec = get_all_states();
		int total = std::accumulate(vec.begin(),vec.end(),0);
        return total;
	}

	double get_t()
	{

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

	mysystem(double b, int V) : graph(V)
	{
		valb = b;
		valt = 0;
	}

	mysystem(double b, graph g) : graph(g)
	{
		valb = b;
		valt = 0;
	}

	void Gillespie(std::ofstream& fout)
	{
		// Set up initial vector
		// Propensities - alpha vector
		// Determine int i_max

		int i_max = 100, reaction;
		double tau, alpha0, gamma;
		std::vector<double> alpha;

		valt = 0.0;

		fout << valt << '\t' << get_I() << std::endl;


		// Loop
		for (int i = 0; i < i_max; i++)
		{
			double r1 = get_rn01();
			double r2 = get_rn01();

			// Compute propensities
			alpha = update_propensity();
			alpha0 = std::accumulate(alpha.begin(),alpha.end(),0.0);

			// Compute tau based on this set of alphas
			tau = get_tau(r1,alpha0);

			// Find next reaction
			// reaction = ;

			// Update state
			update_state(tau,reaction);

			// Output state (to file)
			fout << valt << '\t' << get_I() << std::endl;
			// std::cout << valt << '\t' << get_I() << std::endl;
		}
	}

	// // 2. ACCESS METHODS

	// // Access method for I
	// double get_I()
	// {
	// 	return valI;
	// }

	// // Access method for t
	// double get_t()
	// {
	// 	return valt;
	// }

	// // Access methods for propensities
	
	// std::vector<double> get_alpha()
	// {
	// 	std::vector <double> myvector(3);
	// 	double alpha1 = vala*(valN - valI);
	// 	double alpha2 = valb*(valN - valI)*valI;
	// 	double alpha3 = valc*valI;
 //        myvector = {alpha1, alpha2, alpha3};
	// 	return myvector;
	// }
        
	// double get_alpha0()
	// {
	// 	std::vector <double> temp_vector = get_alpha();
	// 	double tot(0.0);
	// 	for (int i = 0; i < temp_vector.size(); i++)
	// 	{
	// 		tot += fabs(temp_vector[i]);
	// 	}
	// 	return tot;
	// }

	// // Access method for birth rate
	// double get_birth()
	// {
	// 	std::vector <double> temp_vector = get_alpha();
	// 	double tot(0.0);
	// 	for (int i = 0; i < temp_vector.size(); i++)
	// 	{
	// 		if (temp_vector[i] > 0)
	// 		{
	// 			tot += fabs(temp_vector[i]);
	// 		}
	// 	}
	// 	return tot;
	// }

	// // Access method for death rate
	// double get_death()
	// {
	// 	std::vector <double> temp_vector = get_alpha();
	// 	double tot(0);
	// 	for (int i = 0; i < temp_vector.size(); i++)
	// 	{
	// 		if (temp_vector[i] < 0)
	// 		{
	// 			tot += fabs(temp_vector[i]);
	// 		}
	// 	}
	// 	return tot;
	// }
	
	// // 3. MODIFIER METHODS

	// // Update system state
	// void update()
	// {
	// 	double r1(get_rn01()), r2(get_rn01());
	// 	double bd = get_birth() / (get_alpha0());
	// 	valt += get_tau(r1);
	// 	if (r2 >= 0 && r2 < bd)
	// 	{
	// 		valI++;
	// 	}
	// }

	// // 4. PRINT METHODS


};

#endif	// SYSTEM_H

