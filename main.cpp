#include <iostream>
#include <cmath>
#include <vector>

using std::cout;
using std::cin;
using std::endl;
using std::vector;


double function(double x){
    return pow(x,4)-625;
}

double* difference_operators(int n, double h, double x0)
{
    double* x;
    double** operators;
    double* terms;
    
    x = new double[n+2];
    
    for (int i = 0; i < n+2; i++)
    {
        x[i] = x0 + i*h;
    }
    
    operators = new double*[n+1];
    
    for(int i = 0; i < n+1; i++)
    {
        operators[i] = new double[n-i+1];
    }
    
    
    for(int i = 0; i < n+1; i++)
    {
        operators[0][i] = function(x[i+1])-function(x[i]);
    }
    
    
    for (int i = 1; i < n+1; i++)
    {
        for (int j = 0; j < n-i+1; j++)
        {
            operators[i][j] = operators[i-1][j+1]-operators[i-1][j];
        }
    }
    
    terms = new double[n+1];
    
    for (int i = 0; i < n+1; i++)
    {
        terms[i] = operators[i][0];
    }
    
    
    for (int i = 0; i < n+1; i++)
    {
        delete []operators[i];
    }
    
    delete []operators;
    
    
    delete []x;
    
    return terms;
}
double derivative1(double n, double h, double x0)
{
    double* terms;
    double value;
    
    value = 0;
    terms = difference_operators(n, h, x0);
    
    for (int i = 0; i < n; i++)
    {
        value += pow(-1,i)/(double)(i+1)*terms[i];
    }
    
    value = 1/h*value;
    
    
    
    delete []terms;
    
    //cout<<"f'(x0) numerical: "<<value<<endl;
    //cout<<endl;
    return value;
}
double derivative2(double n, double h, double x0){
    double* terms;
    double value=0;
    double* b;
    double* a;
    
    a = new double[n];
    a[0] = 1;
    
    for (int i = 1; i < n; i++)
    {
        b = new double[i+1];
        
        for (int j = 0; j < i+1; j++)
        {
            b[j] = pow(-1,j)/(j+1);
        }
        
        a[i] = 0;
        
        for (int j = 0; j <= i; j++)
        {
            a[i] += b[j]*b[i-j];
        }
        delete []b;
    }
    
    terms = difference_operators(n+1, h, x0);
    
    for (int i = 0; i < n; i++)
    {
        value += a[i]*terms[i+1];
    }
    
    value = 1/(h*h)*value;
    
    delete []terms;
    delete []a;
    //cout<<"f''(x0) numerical: "<<value<<endl;
    return value;
}




//------------------------------------------------------------------------
bool constantpoint(double a, double b, double &constant, double &x){
    if(function(a)*derivative2(10,0.001,a)>0){
        constant = a;
        return true;
    }
    else if(function(b)*derivative2(10,0.001,b)>0){
        constant = b;
        return true;
    }
    else{
        cout<<"error"<<endl;
        return false;
    }
}

double falsi(double a, double b, double accuracy){
    double constant;
    double x, x_prev;
    int iterrations=0;
    if(constantpoint(a,b,constant,x)){
        x_prev = (a-(function(a)/(function(b)-function(a)))*(b-a));
        iterrations++;
        while(fabs(function(x)) >= accuracy){
            x = (x_prev-((function(x_prev)/(function(constant)-function(x_prev)))*(constant-x_prev)));
            x_prev=x;
            cout<<x<<endl;
            iterrations++;
        }
    }
    cout<<"---------Falsi method---------"<<endl;
    cout<<"Solusion: "<<x<<endl;
    cout<<"Amount of iterrations: "<<iterrations<<endl;
    cout<<endl;
    return x;
}
double tangent(double a, double b, double accuracy){
    double constant;
    double x, x_prev;
    int iterrations = 0;
    if(constantpoint(a,b,constant,x)){
        x_prev=(constant-(function(constant)/derivative1(10,0.001,constant)));
        iterrations++;
        while(fabs(function(x)) >= accuracy){
            x=x_prev-(function(x_prev)/derivative1(10,0.001,x_prev));
            x_prev=x;
            cout<<x<<endl;
            iterrations++;
        }
        cout<<"---------Tangent (Newton) method---------"<<endl;
        cout<<"Solution: "<<x<<endl;
        cout<<"Amount of iterrations: "<<iterrations<<endl;
        cout<<endl;
    }
    return x;
}

double secant(double a, double b, double accuracy){
    double constant;
    double x, x1, x2;
    int iterrations=0;
    
    if (constantpoint(a,b,constant,x)){
        x1=constant;
        if(a==constant){
            x2=b;
        }
        else{
            x2=a;
        }
        
        while(fabs(function(x)) >= accuracy){
            x=x1-(function(x1)/(function(x1)-function(x2)))*(x1-x2);
            x2=x1;
            x1=x;
            iterrations++;
        }
    }
    cout<<"---------Secant method--------"<<endl;
    cout<<"Solution: "<<x<<endl;
    cout<<"Amount of iterrations: "<<iterrations<<endl;
    cout<<endl;
    return x;
}

double bisection(double a, double b, double acuracy){
    double x=0;
    int iterrations=0;
    while(fabs(function(x))>=acuracy){
        x=(a+b)/2;
        if(function(a)*function(x)<0){
            b=x;
        }
        if(function(b)*function(x)<0){
            a=x;
        }
        iterrations++;
    }
    cout<<x<<endl;
    cout<<iterrations<<endl;
    return x;
}

double bernoulli(int n, double acuracy){
    double y = 0;
    double yPrev = 0;
    double yFirst = 0;
    vector<double> a;
    a.push_back(2);
    a.push_back(4);
    a.push_back(-3);
    
    y = -(1/a[0])*(a[2]*yFirst-a[0]*yPrev);
    yPrev = y;
    
    
    
    return 0;
}

int main() {
    double a=-10;
    double b=10;
    double acuracy=0.001;
    
    //falsi(a,b,acuracy);
    tangent(a,b,acuracy);
    //secant(a,b,acuracy);
    //bisection(a,b,acuracy);
    
    
    //bernoulli(4,acuracy);
}
