#include <cstdio>
#include <set>
#include <vector>

int main()
{
        int sum = 0, sumf, firstDuplicate;
        std::vector<int> input;
        std::set<int> F;
        auto foundDuplicate = false;

        F.insert(0);
        for(;;){
                int a;
                if(scanf("%d", &a) != 1) break;
                input.push_back(a);
                sum += a;
		auto ins = F.insert(sum);
		if(!ins.second && !foundDuplicate){
			foundDuplicate = true;
			firstDuplicate = sum;
			break;
		}
        }
        sumf = sum;

        while(!foundDuplicate){
                for(auto a : input){
                        sum += a;

                        if(F.count(sum) != 0 && !foundDuplicate){
                                foundDuplicate = true;
                                firstDuplicate = sum;
                                break;
                        }
                }
        }

        printf("Sum of Frequency: %d\n", sumf);

        if(foundDuplicate){
                printf("First duplicate: %d\n", firstDuplicate);
        }else{
                printf("First duplicate: <not found>\n");
        }
        return 0;
}
