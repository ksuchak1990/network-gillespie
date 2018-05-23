#pragma once
#ifndef __SYSTEM_H_INCLUDED__	// Prevents class from being redefined
#define __SYSTEM_H_INCLUDED__

#include <cmath>
#include <iostream>
#include <fstream>

#include "SI_network.h"

class mysystem : public si_net
{
private:
	// 1. DATA MEMBERS

	double valt;

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

public:

	// 1. CONSTRUCTOR METHODS

	// Constructor for 
	mysystem(double b, int V) : si_net(b,V)
	{
		valt = 0;
	}

	mysystem(double b, graph g) : si_net(b,g)
	{
		valt = 0;
	}

	mysystem(si_net m) : si_net(m)
	{
		valt = 0;
	}

	// Run Gillespie algorithm for system
	void Gillespie(std::ofstream& fout, char o)
	{
		// Initial setup
		int reaction;
		int k = 0;
		double tau, alpha0, total, lower, r;
		valt = 0.0;

		int x = si_net::get_size();
		int y = si_net::get_number_react();

		std::vector< std::vector<double> > alpha(x, std::vector<double>(y));

		//Output initial state
		if (o == 'a')
		{
			fout << valt << '\t' << si_net::get_I() << std::endl;
		}
		else if (o == 's')
		{
			fout << valt << '\t' ;
			for (int i = 0; i < x; i++)
			{
				fout << si_net::get_state(i);
			}
			fout << std::endl;
		}

		// int k_max = 300;

		// Loop
		do
		{		
		// for (int k = 0; k < k_max; k++)
		// {
			double r1 = get_rn01();
			double r2 = get_rn01();

			// Compute propensities
			alpha = si_net::update_propensity();

			// Calculate alpha0
			alpha0 = 0.0;

			for (int j = 0; j < y; j++)
			{
				for (int i = 0; i < x; i++)
				{
					alpha0 += alpha[i][j];
				}
			}

			r = r1 * alpha0;

			// Compute tau based on this set of alphas
			tau = get_tau(r2,alpha0);

			// Find next reaction
			total = 0.0;
			lower = 0.0;
			reaction = 0;

			for (int j = 0; j < y; j++)
			{
				for (int i = 0; i < x; i++)
				{
					total += alpha[i][j];
					if (r >= lower && r < total)
					{
						break;
					}
					lower = total;
					reaction++;
				}
			}

			// Update state
			si_net::update_state(reaction);
			valt += tau;

			// Output states (to file)
			if (o == 'a')
			{
				fout << valt << '\t' << si_net::get_I() << std::endl;
			}
			else if (o == 's')
			{
				fout << valt << '\t' ;
				for (int i = 0; i < x; i++)
				{
					fout << si_net::get_state(i);
				}
				fout << std::endl;
			}

		} while (si_net::get_I() > 0 && si_net::get_I() < si_net::get_size());
		// }

	}

};

#endif	// SYSTEM_H

