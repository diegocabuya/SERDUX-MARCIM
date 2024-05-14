
function [t,x] = Rates(params, x0, tf)
    % Function Name: Rates
    % Description:
    % This function simulates the dynamics of a system under cyber-attack. It solves a set of ordinary differential equations (ODEs) describing the evolution of system components over time, considering various 
    % factors such as infection rate, recovery rate, control implementations, and cyber attack parameters.
    % Input:

    % - params: An array containing parameters necessary for the simulation, including:
    %   -  params(1) :  Lambda  - Number of new nodes
    %   -  params(2) :  a  - Parameter for lost of immunity rate (damping)
    %   -  params(3) :  m  - Parameter for lost of immunity rate (cosine's period)
    %   -  params(4) :  beta0  - Malware initial propagation rate
    %   -  params(5) :  mu  - Machine unavailability rate caused by other causes
    %   -  params(6) :  L  - Cyber attack likelihood
    %   -  params(7) :  Psi  - Attack degree, also used in sigma, nabla, and chi
    %   -  params(8) :  eta  - Corrective controls
    %   -  params(9) :  upsilon  - Preventive controls
    %   -  params(10) :  delta  - Attack duration
    %   -  params(11) :  Gamma  - Impact reduction controls
    %   -  params(12) :  LRC  - Likelihood Reduction controls
    %   -  params(13) :  IRC  - Impact Reduction controls for sigma rate, is calculated by the formula Gamma *100
                         
    % -  x0 : An array representing the initial conditions of the system, containing:
    %   -  x0(1) : Initial number of susceptible individuals (S)
    %   -  x0(2) : Initial number of exposed individuals (E)
    %   -  x0(3) : Initial number of recovered individuals (R)
    %   -  x0(4) : Initial number of degraded components (D)
    %   -  x0(5) : Initial number of unavailable components (U)
    %   -  x0(6) : Initial number of destroyed components (X)
    % -  tf : Final time of the simulation
   
    %    Output:
    % -  t : Time points at which the system dynamics are evaluated.
    % -  x : Matrix containing the solution of the ODEs. Each column represents the evolution of a system component (S, E, R, D, U, X) over time.
    

    %---------- Extract parameters from the 'params' array-----------
    Lambda = params(1);
    mu = params(5); 

    % Time interval
    time = 0:0.01:tf;

    % Parameters for rates calculation

    %Omega- Loss of resistance rate  
    a = params(2);  % Parameter for lost of immunity rate (damping)
    m = params(3);  % Parameter for lost of immunity rate (cosine's period)

    %beta - Propagation Rate
    beta0 = params(4); % Malware initial propagation rate
    L = params(6); %cyber attack likelihood
    Psi = params(7); % cyber attack degree, also used in sigma, nabla and chi.
    xi = 2/ ((1/L) + (1/Psi));

    %phi -Sanitation Rate
    eta = params(8); % Corrective controls
    
    
    %gamma- Recovery Rate
    upsilon = params(9);  % Preventive controls
    
    
    %sigma -Cyber Attack (Degraded) rate 

    delta = params(10);%attack duration, also used in nabla and chi. 
    Gamma = params(11);% Impact reduction controls
    LRC  = params(12);% Likelihood Reduction controls: slope of the first sigmoid
    IRC = params(13);% Impact Reduction controls: slope of the second sigmoid
    
    % Define the sigmoid function 
    sigmoid = @(a,Psi,Gamma, x) (Psi*(1-Gamma)) ./ (1 + exp(-a * x));
    
    %a : Control speed. 
    %psi*(1-gamma): controls height. 
    
    % Parameters
    Theta= delta*(1-Gamma);
    
   
    i_p = 0; %initial_point
    f_p = tf*Theta; % Final point
    b = tf*Theta- tf/4; % % Midpoint of the sigmoids
    
    % Calculate the values of nabla and chi
    
    %Nabla - Cyber Attack Execution - Unavailable
    if (Psi>=0.5) && (delta<= 0.5) 
        
        nabla = 2/((1/Psi)+(1/delta)); 
        %fprintf('El valor de nabla es: %f\n', nabla);
    else
        nabla = 0; 
    end
    
    %chi - Cyber Attack Execution - Destroy
    if (Psi>=0.5) && (delta>0.5) 
        
        chi = 1/((1/Psi)+(1/delta)); 
        %fprintf('El valor de chi es: %f\n', chi);
    else
        chi = 0; 
    end
    
  
    % Solve the ODEs using ode45 solver
    [t, x] = ode45(@(t,x)[Lambda + (abs(exp(-a*t)*cos(2*pi*t/m)))*x(3) - beta0/ (1+((1-xi) * ((x(4)+x(5)+x(6))  )))*( x(1) ) - mu*x(1) - (upsilon/2) *(1- tanh(3*(x(1)+x(2))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6))))*x(1); ... 
                          beta0/ (1+((1-xi)* ((x(4)+x(5)+x(6)) )))*(x(1) ) - (sigmoid(LRC, Psi, Gamma, t - b - i_p) - sigmoid(IRC, Psi, Gamma, t - b - f_p))*x(2) - mu*x(2) - (upsilon/2) *(1- tanh(3* (x(1)+x(2))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6))))*x(2); ...
                          (eta/2)*tanh(3*(x(4)+x(5)+x(6))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)))*(x(4)+x(5)) + (upsilon/2)*(1-tanh(3*(x(1)+x(2))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6))))*(x(1) +x(2)) - (abs(exp(-a*t)*cos(2*pi*t/m)))*x(3) - mu*x(3); ...
                          (sigmoid(LRC, Psi, Gamma, t - b - i_p) - sigmoid(IRC, Psi, Gamma, t - b - f_p))*x(2) - ((eta/2)*tanh(3*(x(4)+x(5)+x(6))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)))*x(4)) - mu*x(4) - nabla*x(4)  - chi*x(4) ; ...
                          mu*(x(1) + x(2) + x(3) + x(4)) + nabla*x(4) - nabla*x(5) - (eta/2)*tanh(3*(x(4)+x(5)+x(6))/(x(1)+x(2)+x(3)+x(4)+x(5)+x(6)))*x(5); ...
                          chi*x(4) + chi*x(5);], time, x0);

end





