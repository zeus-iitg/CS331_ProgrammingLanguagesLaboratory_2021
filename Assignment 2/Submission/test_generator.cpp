#include<bits/stdc++.h>

using namespace std;

ofstream fout;

vector<long long> account_numbers_vector[10];

map<long long, double> balance[10];

int numAccounts[10];

int cnt;

double random_double_generator(){
	int upper_bound = 10000;
	int temp1 = rand()%upper_bound;
	int temp2 = rand()%100;
	double a_random_double = (double)temp1 + ((double)temp2)/100;
	return a_random_double;
}

string make_len_10(long long num){
	string s = to_string(num);
	if(s.length() == 10)
		return s;
	else{
		reverse(s.begin(), s.end());
		while(s.length() < 10){
			s.push_back('0');
		}
		reverse(s.begin(), s.end());
		return s;
	}
}

void process(int type, int branch_num, int limit = 0){
	if(type == 1){
		int idx = rand()%account_numbers_vector[branch_num].size();
		double amount = random_double_generator();
		balance[branch_num][account_numbers_vector[branch_num][idx]] = balance[branch_num][account_numbers_vector[branch_num][idx]] + amount;
		fout<<type<<" "<<make_len_10(account_numbers_vector[branch_num][idx])<<" "<<amount<<"\n";
		cnt++;
	}
	if(type == 2){
		int idx = rand()%account_numbers_vector[branch_num].size();
		double amount = random_double_generator();
		if(balance[branch_num][account_numbers_vector[branch_num][idx]] >= amount)
			balance[branch_num][account_numbers_vector[branch_num][idx]] = balance[branch_num][account_numbers_vector[branch_num][idx]] - amount;
		fout<<type<<" "<<make_len_10(account_numbers_vector[branch_num][idx])<<" "<<amount<<"\n";
		cnt++;
	}
	if(type == 3){
		for(int i = 0; i < 10; i++){
			int lim = limit / 10;
			int ctr = 0;
			while(ctr < lim){
				int idx1 = rand()%account_numbers_vector[branch_num].size();
				int idx2 = rand()%account_numbers_vector[i].size();
				if(i == branch_num && idx1 == idx2)
					continue;
				double amount = random_double_generator();
				if(balance[branch_num][account_numbers_vector[branch_num][idx1]] >= amount){
					balance[branch_num][account_numbers_vector[branch_num][idx1]] = balance[branch_num][account_numbers_vector[branch_num][idx1]] - amount;
					balance[i][account_numbers_vector[i][idx2]] = balance[i][account_numbers_vector[i][idx2]] + amount;
				}
				fout<<type<<" "<<make_len_10(account_numbers_vector[branch_num][idx1])<<" "<<make_len_10(account_numbers_vector[i][idx2])<<" "<<amount<<"\n";
				ctr++;
				cnt++;
			}
		}
	}
	if(type == 4){
		long long branch = branch_num;
		long long num = numAccounts[branch] + 1;
		numAccounts[branch]++;
		num = num + 1000000000ll * branch;
		account_numbers_vector[branch].push_back(num);
		double amount = random_double_generator();
		balance[branch][num] = amount;
		fout<<type<<" "<<branch<<" "<<amount<<"\n";
		cnt++;
	}
	if(type == 5){
		for(long long u:account_numbers_vector[branch_num]){
			if(balance[branch_num].find(u) != balance[branch_num].end()){
				fout<<type<<" "<<make_len_10(u)<<"\n";
				cnt++;
			}
		}
	}
	if(type == 6){
		int ctr = 0;
		int lim = limit / 9;
		while(ctr < lim){
			for(int source_branch = 0; source_branch < 10; source_branch++){
				for(int dest = 0; dest < 10; dest++){
					if(dest == source_branch)
						continue;
					int idx;
					bool found = false;
					while(!found){
						idx = rand()%account_numbers_vector[source_branch].size();
						if(balance[source_branch].find(account_numbers_vector[source_branch][idx]) != balance[source_branch].end()){
							found = true;
						}
					}
					long long num = numAccounts[dest] + 1;
					numAccounts[dest]++;
					long long new_account_number = dest * 1000000000ll + num;
					balance[dest][new_account_number] = balance[source_branch][account_numbers_vector[source_branch][idx]];
					account_numbers_vector[dest].push_back(new_account_number);
					balance[source_branch].erase(account_numbers_vector[source_branch][idx]);
					fout<<type<<" "<<make_len_10(account_numbers_vector[source_branch][idx])<<" "<<dest<<"\n";
					cnt++;
				}
			}
			ctr++;
		}
	}
}

int lim, ctr;

int main(){
	memset(numAccounts, 0, sizeof numAccounts);
	srand(time(0));
	fout.open("testcase.txt");
	int n = 1000050;
	fout<<n<<"\n";
	lim = 300;
	cnt=0;
	for(int i = 0; i < 10; i++){
		ctr = 0;
		while(ctr < lim){
			process(4, i);
			ctr++;
		}
	}
	cout<<"add accounts complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	lim = 33000;
	cnt=0;
	for(int i = 0; i < 10; i++){
		ctr = 0;
		while(ctr < lim){
			process(1, i);
			ctr++;
		}
	}
	cout<<"deposit complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	lim = 33000;
	cnt=0;
	for(int i = 0; i < 10; i++){
		ctr = 0;
		while(ctr < lim){
			process(2, i);
			ctr++;
		}
	}
	cout<<"withdraw complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	lim = 33000;
	cnt=0;
	for(int i = 0; i < 10; i++){
		process(3, i, lim);
	}
	cout<<"transfer money complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	lim = 405;
	cnt=0;
	process(6, -1, lim);
	cout<<"transfer accounts complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	lim = 300;
	cnt=0;
	for(int i = 0; i < 10; i++){
		process(5, i, lim);
	}
	cout<<"delete accounts complete\n";
	/*
	for(int i = 0; i < 10; i++){
		cout<<balance[i].size()<<" ";
	}
	cout<<"\n";
	cout<<cnt<<"\n";
	*/

	fout.close();
	
	return 0;
}