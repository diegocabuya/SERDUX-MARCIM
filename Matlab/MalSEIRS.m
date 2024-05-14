function [t,x] = MalSEIRS(beta0,mu,alpha,sigma,phi,x0,tf,p,lambda,c,a,m,xi)

%Time inteval
time = 0:0.01:tf; 

%Solution with ode45 of the model, given its initial condition (x0) and its interval 
[t,x] = ode45(@(t,x)[p*lambda-(beta0/(1+xi*x(3)))*x(1)*x(3)+(abs(exp(-a*t)*cos(2*pi*t/m)))*x(4)-mu*x(1) - phi*x(1); ...
    (beta0/(1+xi*x(3)))*x(1)*x(3)-sigma*x(2)-mu*x(2) ; ...
    sigma*x(2)-(tanh(x(3)/c))*x(3)-(mu+alpha)*x(3) ; ...
    (1-p)*lambda + (tanh(x(3)/c))*x(3)-(abs(exp(-a*t)*cos(2*pi*t/m)))*x(4)-mu*x(4) + phi*x(1);], time, x0);


end
