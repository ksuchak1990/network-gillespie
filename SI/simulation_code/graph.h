#pragma once
#ifndef __GRAPH_H_INCLUDED__	// Prevents class from being redefined
#define __GRAPH_H_INCLUDED__

#include <iostream>
#include <fstream>
#include <vector>
#include <list>

class graph
{
private:
    std::vector< std::list<int> > adjlist;
    std::vector<int> states;

public:

    // CONSTRUCTORS

    // Constructor for V vertices
    graph(int V)
    {
        adjlist.resize(V);
        states.resize(V);
    }

    graph(const graph& g_other)
    {
        std::vector< std::list<int> > x = g_other.get_adj_list();
        std::vector<int> y = g_other.get_all_states();
        adjlist = x;
        states = y;
    }

    graph(const std::vector< std::list<int> >& adj, const std::vector<int>& st)
    {
        adjlist = adj;
        states = st;
    }



    // MODIFIERS

    // Add edge (u,v)
    void add_edge(int u, int v)
    {
        adjlist[u].push_back(v);
        adjlist[v].push_back(u);
    }

    // Set state of vertex n to state s
    void set_state(int n, int s)
    {
        states.at(n) = s;
    }

    // Set state of all vertices to state s
    void set_state(int s)
    {
        int N = states.size();
        for (int j = 0; j < N; j++)
        {
            states.at(j) = s;
        }
    }

    // Make graph complete
    void make_complete()
    {
    	int N = adjlist.size();
		for (int i = 0; i < N; i++)
		{
			for (int j = 0; j < N; j++)
			{
				if (i != j)
				{
					adjlist[i].push_back(j);
				}
			}
		}
    }

    void make_star()
    {
        int N = adjlist.size();
        for (int i = 1; i < N; i++)
        {
            adjlist[i].push_back(0);
            adjlist[0].push_back(i);
        }        
    }

    void make_line()
    {
        int N = adjlist.size();
        adjlist[0].push_back(1);
        adjlist[N-1].push_back(N-2);
        for (int i = 1; i < N-1; i++)
        {
            adjlist[i].push_back(i-1);
            adjlist[i].push_back(i+1);
        }        
    }


    // ACCESSORS

    // Get list of vertices adjacent to vertex n
    std::list<int> get_neighbours(int n)
    {
		std::list<int> mylist = adjlist.at(n);
		return mylist;
    }

    // Get state of vertex n
    int get_state(int n)
    {
        int x = states.at(n);
        return x;
    }

    // Get list of states of vertices adjacent to vertex n
    std::list<int> get_adj_state(int n)
    {
        std::list<int> k = get_neighbours(n);
        std::list<int>::iterator listiter;
        std::list<int> mylist;
        int x;

        for (listiter = k.begin(); listiter != k.end(); listiter++)
        {
            x = get_state(*listiter);
            mylist.push_back(x);
        }
        return mylist;
    }

    // Get adjacency list
    const std::vector< std::list<int> >& get_adj_list() const
    {
        return adjlist;
    } 

    // Get vector of states
    const std::vector<int>& get_all_states() const
    {
        return states;
    }

    // Get size of network
    int get_size()
    {
        int x = adjlist.size();
        return x;
    }

    // Print graph to console
    void print()
    {
        std::list<int> k;
        std::list<int>::iterator listiter;
        int N = adjlist.size();

        // Print adjacency list
        for (int i = 0; i < N; i++)
        {
            k = get_neighbours(i);

            for (listiter = k.begin(); listiter != k.end(); listiter++)
            {
                std::cout << *listiter;
            }
            std::cout << std::endl;
        }

        std::cout << std::endl;

        // Print states
        for (int i = 0; i < N; i++)
        {
            std::cout << states.at(i); 
        }

        std::cout << std::endl;
    }

    // Print graph to file
    void print(std::ofstream& fout1, std::ofstream& fout2)
    {
        std::list<int> k;
        std::list<int>::iterator listiter;
        int N = adjlist.size();

        // Print adjacency list
        for (int i = 0; i < N; i++)
        {
            k = get_neighbours(i);

            for (listiter = k.begin(); listiter != k.end(); listiter++)
            {
                if (listiter != k.begin())
                {
                    fout1 << '\t';
                }
    
                fout1 << *listiter;
            }

            fout1 << std::endl;

        }

        // Print states
        for (int i = 0; i < N; i++)
        {
            if (i != 0)
            {
                fout2 << '\t';
            }

            fout2 << states.at(i); 
        }
        fout2 << std::endl;

    }



};
#endif	// GRAPH_H

