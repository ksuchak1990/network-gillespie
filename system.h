#pragma once
#ifndef __SYSTEM_H_INCLUDED__	// Prevents class from being redefined
#define __SYSTEM_H_INCLUDED__

#include <fstream>
#include <cstdlib>
#include <cmath>
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
	double get_tau(double r)
	{
		return (1/get_alpha0()) * log(1/r)  ;
	}

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
    		        // x = get_state(*listiter);
    	    	    // mylist.push_back(x);
    	    	}
				
				// std::list<int> mylist = get_neighbours(i);
				// int M = mylist.size();
				// for (int j = 0; j < M; j++)
				// {
				// 	if (get_state(j) == 1)
				// 	{
				// 		props[i] += valb;
				// 	}
				// }
			}
		}
		return props;
	}

	void update_state()
	{

	}

	double get_alpha0()
	{

	}

	int get_I()
	{
		// std::vector<int> vec = 

	}

	double get_t()
	{

	}

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

	void Gillespie()
	{
		// Set up initial vector
		// Propensities - alpha vector

		// Determine int i_max
		int i_max;

		// Loop
		for (int i = 0; i < i_max; i++)
		{
			double r1 = get_rn01();
			double r2 = get_rn01();

			// Compute propensities
			// Write a function to do this

			// Compute tau based on this set of alphas

			// Find next reaction for each vertex

			// Update state

			// Output state

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

	// // Print state to screen
	// void state()
	// {
	// 	std::cout << valt << '\t' << valI << std::endl;
	// }

	// // Print state to file
	// void state(std::ofstream& fout)
	// {
	// 	fout << valt << '\t' << valI << std::endl;
	// }

};

#endif	// SYSTEM_H

