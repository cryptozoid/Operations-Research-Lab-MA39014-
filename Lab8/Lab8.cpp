#include <iostream>
#include <bits/stdc++.h>


using namespace std;

int vecminlocVertical(int** cost, int j,int m, int n){
	int idx=-1,min=1000;
	for(int i=0;i<m;i++){
		if ((cost[i][j]<min)&&(cost[i][n]!=-1)){
			min=cost[i][j];
			idx=j;
		}
	}
	//cout<<"local MIN j="<<idx<<endl;
	return idx;
}

int vecminloc(int** cost, int i,int m, int n){
	int idx=-1,min=1000;
	for(int j=0;j<n;j++){
		if ((cost[i][j]<min)&&(cost[m][j]!=-1)){
			min=cost[i][j];
			idx=j;
		}
	}
	//cout<<"local MIN j="<<idx<<endl;
	return idx;
}

int findinitloc(int** cost,int m,int n){
	int i=-1,j=-1,temp=1000;	
	for (int it=0;it<m;it++){
		if (cost[it][n]!=-1){
			int jt=vecminloc(cost,it,m,n);
			if (cost[it][jt]<temp){
				temp = cost[it][jt];
				i=it;
				j=jt;
			}
		}
	}
	//cout<<"MIN i="<<i<<endl;
	return (1000*i + j);
}

int vecsum(int* s, int m){ //m should be n
	int temp=0;
	for (int i=0;i<m;i++){
		temp += s[i];
	}
	return temp;
}

int finished(int** tableau,int m,int n){
	return (vecsum(tableau[m],n)==0);
}

int checkbal(int* supply,int* demand,int m, int n){
	int sumS=0,sumD=0;
	sumS=vecsum(supply,m);
	sumD=vecsum(demand,n);
	cout<<"sumS = "<<sumS<<" sumD = "<<sumD<<endl;
	return (sumS==sumD);
}

void maketableau(int** tableau,int* supply,int m,int* demand,int n){
	for (int i =0;i<m;i++){
		tableau[i][n]=supply[i];
	}
	for (int j =0;j<n;j++){
		tableau[m][j]=demand[j];
	}
	return;
}

void printtableau(int** tableau, int m, int n, int** cost,int itr){
	cout<<"\n\nIteration #"<<itr<<endl;
	int i,j,sol=0;
	for (i=0;i<(m+1);i++){
		if (i==m){
			//cout<<" =("<<cost[i][j]<<")";
			cout<<endl;			
		}
		for (j=0;j<(n+1);j++){
			cout<<tableau[i][j]<<"\t";
			if (j==n){
				cout<<"             ";
				//cout<<" =("<<cost[i][j]<<")";		
			}
		}
		cout<<endl;
		if (i==m){
			for (int j=0;j<(n);j++){
				cout<<" =("<<cost[i][j]<<")\t";
			}
			cout<<endl;			
		}
		if (i<m){
			for (int j=0;j<(n);j++){
				cout<<" *"<<cost[i][j]<<"\t";
				sol += cost[i][j]*tableau[i][j];
			}
			tableau[m][n]=sol;
			cout<<" =("<<cost[i][n]<<")";
			cout<<endl;
		}
	}
	tableau[m][n]=sol;
	cout<<"---------------------"<<endl;
	return;
}

int cancel(int** tableau,int** cost, int m, int n, int i, int j){
	if (tableau[m][j]<tableau[i][n]){
		cost[m][j]=-1;
		return 1; //crossout column
	}
	if (tableau[m][j]>tableau[i][n]){
		cost[i][n]=-1;
		return 2; //crossout row
	}
	else{ 
		cost[m][j]=-1;
		cost[i][n]=-1;
		return 3; //crossout both
	}
}

int NWCR(int** tableau,int i,int j,int m,int n,int** path,int itr,int** cost){	
	path[itr][0]=i+1;
	path[itr][1]=j+1;		
	path[itr][2]=min(tableau[m][j],tableau[i][n]);
	int temp=cancel(tableau,cost,m,n,i,j);
	tableau[i][j]=path[itr][2];
	tableau[m][j]-=path[itr][2];
	tableau[i][n]-=path[itr][2];
	//cout<<"Action= "<<temp<<endl;
	return temp;
}

void printpath(int** path,int m,int n,int itr){
	for (int k=0; k<(itr);k++){
		cout<<"("<<path[k][0]<<","<<path[k][1]<<")";
		if (k!=(itr-1)){
			cout<<" -> ";
		}
		else{
			cout<<endl;
		}
	}
	return;
}



int main(){
	cout<<"Hello, lets solve for the BFS (Phase-I Solution) of a Balanced Transportation Method: "<<endl;
	cout<<"Enter the algorithm that you want to solve with:\n 1. North West Corner Rule (NWCR)\n 2. Least Count Method (LCM)\n";
	int algo_choice,m,n,i,j,itr=0;
	cin>>algo_choice;
	cout<<"Please enter the number of Supply Sources (m) = ";
	cin>>m;
	cout<<"Please enter the number of Demand Destinations (n) = ";
	cin>>n;

	int** cost = (int**)malloc((m+1)*sizeof(int*));
	for(i=0;i<(m+1);i++){
		cost[i]=(int*)malloc((n+1)*sizeof(int));	
		for (j=0;j<(n+1);j++){
			cost[i][j]=0;
		}
	}

	int* supply = (int*)malloc(m*sizeof(int));
	int* demand = (int*)malloc(n*sizeof(int));

	cout<<"Enter the Supply Costs for each source: ";
	for (i=0;i<m;i++){
		cin>>supply[i];
	}
	cout<<"Enter the Demand Costs for each destination: ";
	for (j=0;j<n;j++){
		cin>>demand[j];
	}
	
	if (checkbal(supply,demand,m,n)){
		cout<<"The problem is Balanced... you may proceed\n";
	}
	else{
		cout<<"The problem is NOT BALANCED, please try again...\n";
		return 0;
	}

	cout<<"Enter the details of the Cost Matrix (mxn): \n";
	for (i=0;i<m;i++){
		for (j=0;j<n;j++){
			cin>>cost[i][j];
		}
	}
	cout<<endl;	
	
	int** tableau= (int**)malloc((m+1)*sizeof(int*));
	for (i=0;i<(m+1);i++){
		tableau[i]=(int*)malloc((n+1)*sizeof(int));
		for (j=0; j<(n+1);j++){
			tableau[i][j]=0;
		}	
	}
	
	int** path = (int**)malloc((m+n-1)*sizeof(int*));
	for (i=0;i<(m+n-1);i++){
		path[i]=(int*)malloc(3*sizeof(int));
	}

	maketableau(tableau,supply,m,demand,n);
	printtableau(tableau,m,n,cost,itr);

	if (algo_choice==1){
		i=0,j=0;
		int actionIndic=0;
		while(!finished(tableau,m,n)){
			actionIndic=0;
			actionIndic=NWCR(tableau,i,j,m,n,path,itr,cost);
			if (actionIndic==1){
				j++;
			}
			else{
				i++;
			}			
			itr++;
			printtableau(tableau,m,n,cost,itr);
		}
		cout<<"\n\n Finished Transportation Problem!\n Minimum Cost ="<<tableau[m][n]<<"\n";
		printpath(path,m,n,itr);
		return 0;
	}
	else{	
		int minloc=findinitloc(cost,m,n);
		i=int(minloc/1000),j=minloc-(i*1000);
		//cout<<" i="<<i<<", j="<<j<<endl;
		int actionIndic=0;
		while(!finished(tableau,m,n)){
			actionIndic=0;
			actionIndic=NWCR(tableau,i,j,m,n,path,itr,cost);
			
			if (actionIndic==1){
				j=vecminloc(cost,i,m,n);
				//minloc=1000*i + j;
			}
			else if (actionIndic==2){
				i=vecminlocVertical(cost,j,m,n);
				//minloc=1000*i + j;
			}
			else {
				//find new starting point, except crossed out rows
				minloc=findinitloc(cost,m,n);
				i=int(minloc/1000),j=minloc-(i*1000);
			}
			//cout<<" i="<<i<<", j="<<j<<endl;			
			itr++;
			printtableau(tableau,m,n,cost,itr);
		}
		cout<<"\n\n Finished Transportation Problem!\n Minimum Cost ="<<tableau[m][n]<<"\n";
		printpath(path,m,n,itr);

		return 0;
	}

}
