#include <vector>
#include <iostream>
#include <cstdint>

int algo(std::vector<int> init, int n)
{
	std::vector<std::int32_t> arr(n, 0);

	int i = 1;
	for(auto &lastValue: init)
	{
		arr[lastValue] = i++;
	}

	int turnNumber = init.size() + 1;
	int lastSpokenNumber = init[init.size() - 1];

	n -= init.size();

	while(n > 0)
	{
		auto lastTurn = arr[lastSpokenNumber];
		arr[lastSpokenNumber] = turnNumber - 1;

		lastSpokenNumber = ((lastTurn > 0) - (lastTurn < 0)) * (turnNumber - lastTurn - 1);

		turnNumber++;

		--n;
	}

	/*
	auto seen = 0;
	for(auto &i : arr)
	{
		if(i != -1)
			seen++;
	}

	std::cout << seen * 100 / arr.size() << std::endl;
	*/

	return lastSpokenNumber;
}

int main()
{
	std::cout << algo({2, 0, 6, 12, 1, 3}, 30000000) << std::endl;
}
