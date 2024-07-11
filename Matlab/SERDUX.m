
%Clear the workspace, command window, and close all figures
clear; 
clc;
close all;



%%Initials Conditions 
x0 = [60; % Susceptible (S)
      9;   % Exposed (E)
      0;  % Resistant (R)
      2;   % Degraded (D)
      0;   % Unavailable(U)
      0];   % Destroyed (X)

% Final time of the simulation 
tf = 35;


% % Model's parameters

lambda = 0;  % Number of new nodes

a = 0.2; % Parameter for lost of immunity rate (damping)
m = tf/8; % Parameter for lost of immunity rate (cosine's period)

beta0 = 0.8;  % Malware initial propagation rate
mu = 0.001;    % Machine unavailability rate caused by other causes


eta = 0.5; % corrective controls (η)
upsilon = 0.5; % preventive controls (υ)
zeta = 0.5; %detective controls (ζ)
Omega = 0.5; % Compensatory Controls Implementation (Ω)
alpha = 0.5; %Deterrent Control Implementation(α)
iota = (3)/( (1/Omega) + (1/alpha) + (1/zeta)) ; % Likelihood Reduction controls(ι)

L= 0.6;%cyber attack likelihood (L)
psi= 0.6; %attack degree (Ψ)
delta = 0.6;%attack duration(δ)

Gamma = 3/((1/(eta))+ 1/(upsilon) + 1/(zeta)); %impact reduction controls (Γ)
IRC = Gamma*100; %Impact Reduction controls*100 to apply in the sigma formula. 



% Store parameters in an array 
params = [lambda, a,m ,beta0, mu,L,psi, eta,upsilon, delta,Gamma,iota*100,IRC];

%solve the ODEs and return the time points (t) and the corresponding solution matrix (x).
[t,x]= Rates(params,x0, tf);
h=figure; 

% Customize plot colors
newcolors = [
    0 0.3 1;%Blue
    0.9784 0.5686 0.3058;% Yellow
    0.14 0.39 0.42; %green
    0.47 0.17 0.5294; % purple
    
    1 0 0; %red
    
    0 0 0; %black
];

% Plot the results, where each variable (S, E, R, D, U, X) is plotted against time (t). 
plot(t, x(:,1), 'LineWidth', 2)
hold on
for i=2:6
    plot(t, x(:,i), 'LineWidth', 2)
end 

% Label axes
xlabel('Time units', 'FontSize', 12)
ylabel('Number of individuals', 'FontSize', 12)
% Add legend
legend('Susceptible (S)', 'Exposed (E)', 'Resistant (R)', 'Degraded (D)', 'Unavailable (U)', 'Destroyed(X)','NumColumns',3,'fontsize',15,'Location', 'southoutside')
% Set custom colors
colororder(newcolors);
% Set figure properties for printing
set(h,'Units','Inches');
pos = get(h,'Position');
set(h,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
% Save the plot as a PDF file named "SERDUX"
print(h,'SERDUX','-dpdf','-r0')



