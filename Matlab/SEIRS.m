function [t,x]= SEIRS(beta,gama,mu,alpha,sigma,omega,x0,tf,popTotal)

%Time interval
time = 0:0.01:tf; 

%Solution with ode45 of the model, given its initial condition (x0) and its interval 
[t,x] = ode45(@(t,x)[mu*popTotal-beta*x(1)*x(3)+omega*x(4)-mu*x(1); ...
    beta*x(1)*x(3)-sigma*x(2)-mu*x(2) ; ...
    sigma*x(2)-gama*x(3)-(mu+alpha)*x(3) ; ...
    gama*x(3)-omega*x(4)-mu*x(4);], time, x0);




end