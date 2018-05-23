#pragma once
#ifndef __SYSTEM_H_INCLUDED__	// Prevents class from being redefined
#define __SYSTEM_H_INCLUDED__

#include <cmath>
#include <iostream>
#include <fstream>

#include "SIS_network.h"

class mysystem : public sis_net
{
private:
	// 1. DATA MEMBERS

	double valt;			// Time variable

	// 2. ACCESS METHODS

	// Random number - 0<r<1
	double get_rn01()
	{
		return ((double)rand() / (RAND_MAX));
	}

	// Get time until next change, tau
	double get_tau(double r, double alpha0)
	{
		return (1/alpha0) * log(1/r)  ;
	}

public:

	// 1. CONSTRUCTOR METHODS

	// Constructor given model parameters and number of nodes
	mysystem(double b, double gam, int V) : sis_net(b,gam,V)
	{
		valt = 0;
	}

	// Constructor given model parameters and graph
	mysystem(double b, double gam, graph g) : sis_net(b,gam,g)
	{
		valt = 0;
	}

	// Constructor given model
	mysystem(sis_net m) : sis_net(m)
	{
		valt = 0;
	}

	// Run Gillespie algorithm for system
	void Gillespie(std::ofstream& fout, char o, char f)
	{
		// Initial setup
		int reaction;
		int k(0);		// Loop counter
		double tau, alpha0, total, lower, r;
		valt = 0.0;
		int k_max = get_size() * 20;

		int x = get_size();
		int y = get_number_react();

		std::vector< std::vector<double> > alpha(x, std::vector<double>(y));

		//Output initial state
		if (o == 'a')
		{
			fout << valt << '\t' << get_I() << std::endl;
		}
		else if (o == 's')
		{
			fout << valt << '\t' ;
			for (int i = 0; i < x; i++)
			{
				fout << get_state(i);
			}
			fout << std::endl;
		}

		if (f == 'a')		// Run to absorbing state
		{
			// Loop
			do
			{
				double r1 = get_rn01();
				double r2 = get_rn01();

				// Compute propensities
				alpha = update_propensity();

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
				update_state(reaction);
				valt += tau;

				// Output states (to file)
				// Output aggregate
				if (o == 'a')
				{
					fout << valt << '\t' << get_I() << std::endl;
				}
				// Output individual states
				else if (o == 's')
				{
					fout << valt << '\t' ;
					for (int i = 0; i < x; i++)
					{
						fout << get_state(i);
					}
					fout << std::endl;
				}

				k++;

			} while (get_I() > 0 && get_I() < get_size());
		}

		else if (f == 'm')		// Run to max number of loops
		{
			// Loop
			do
			{
				double r1 = get_rn01();
				double r2 = get_rn01();

				// Compute propensities
				alpha = update_propensity();

				// Calculate alpha0
				alpha0 = 0.0;

				for (int j = 0; j < y; j++)
				{
					for (int i = 0; i < x; i++)
					{
						alpha0 += alpha[i][j];
					}
				}

				if (alpha0 == 0)
				{
					break;
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
				update_state(reaction);
				valt += tau;

				// Output states (to file)
				// Output aggregate
				if (o == 'a')
				{
					fout << valt << '\t' << get_I() << std::endl;
				}
				// Output individual states
				else if (o == 's')
				{
					fout << valt << '\t' ;
					for (int i = 0; i < x; i++)
					{
						fout << get_state(i);
					}
					fout << std::endl;
				}

				k++;

			} while (k < k_max);
		}

	}
	
};

#endif	// SYSTEM_H

