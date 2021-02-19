#include <array>
#include <bitset>
#include <iostream>
#include <thread>
#include <vector>
#include <cstring>
#include <cstdlib>
#include "gen.h"
#include "hash.h"
#include "perft.h"
#include "piece.h"
#include "pos.h"
#include "search.h"
#include "square.h"
#include "string.h"
#include "types.h"
#include "uci.h"
using namespace std;

int main(int argc, char *argv[])
{
	uci_init();
	piece_init();
	gen_init();
	hash_init();
	uci_init();

	if (argc >= 2) {
		if (strcmp(argv[1], "-p") == 0)
			perft_validate(argc - 2, argv + 2);
		else if (strcmp(argv[1], "-h") == 0)
			hash_validate(argc - 2, argv + 2);

		return EXIT_SUCCESS;
	}

    //setvbuf(stdin, nullptr, _IONBF, 0);
    //setvbuf(stdout, nullptr, _IONBF, 0);
	
	thread search_thread(search_init);
		
	uci_loop();
		
	search_thread.join();

	cerr << "Exiting!" << endl;

	return EXIT_SUCCESS;
}


