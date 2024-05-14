
%%Initials Conditions 
x0_SEIRS = [60; 9; 2; 0]; 
x0 = [60; % Susceptible (S)
      9;   % Exposed (E)
      0;  % Recover (R)
      2;   % Degraded (D)
      0;   % Unavailable(U)
      0];   % Destroyed (X)

% Model's parameters
tf = 35;
lambda = 0;  % Number of new nodes

a = 0.2; % Parameter for lost of immunity rate (damping)
m = tf/8; % Parameter for lost of immunity rate (cosine's period)

beta0 = 0.8;  % Malware initial propagation rate
mu = 0.0001;    % Machine unavailability rate caused by other causes
%Exclusive SERDUX Parameteres

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

%SEIRS Parameters
beta = beta0;
omega = a;
gama = eta;
alpha_SEIRS = 1/((1/psi)+(1/delta));
sigma = (1-Gamma)*psi;


% Graph 
h=figure;
[t,x_SEIRS]=SEIRS(beta,gama,mu,alpha_SEIRS,sigma,omega,x0_SEIRS,tf,33);
params = [lambda, a,m ,beta0, mu,L,psi, eta,upsilon, delta,Gamma,iota*100,IRC];


[t,x_Serdux]= Rates(params,x0, tf);
%Graph
newcolors = [
    0 0.3 1;%Blue
    0.9784 0.5686 0.3058;% Yellow
    0.14 0.39 0.42; %green
    0.47 0.17 0.5294; % purple
    
    1 0 0; %red
    
    0 0 0; %black
];


%S
plot(t, x_Serdux(:,1), 'LineWidth', 2,'Color',[0 0.3 1],'LineStyle','--')
hold on
plot(t, x_SEIRS(:,1), 'LineWidth', 2,'Color',[0 0.3 1],'LineStyle','-')
%E
plot(t, x_Serdux(:,2), 'LineWidth', 2,'Color',[0.9784 0.5686 0.3058],'LineStyle','--')
plot(t, x_SEIRS(:,2), 'LineWidth', 2,'Color',[0.9784 0.5686 0.3058],'LineStyle','-')
%I
plot(t, x_Serdux(:,4), 'LineWidth', 2,'Color',[1 0 0 ],'LineStyle','--')
plot(t, x_SEIRS(:,3), 'LineWidth', 2,'Color',[1 0 0 ],'LineStyle','-')
%R
plot(t, x_Serdux(:,3), 'LineWidth', 2,'Color',[0.14 0.39 0.42],'LineStyle','--')
plot(t, x_SEIRS(:,4), 'LineWidth', 2,'Color',[0.14 0.39 0.42],'LineStyle','-')



xlabel('Time units', 'FontSize', 12)
ylabel('Number of nodes', 'FontSize', 12)


legend('Susceptible (S)', 'Susceptible (S)', 'Exposed (E)', 'Exposed (E)', ...
    'Degraded (D)','Infected(I)','Recovered (R)','Recovered (R)','NumColumns',4,'fontsize',12, 'Location', 'southoutside')
% Generates PDF version of graph
set(h,'Units','Inches');
pos = get(h,'Position');
set(h,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(h,'SEIRS vs SERDUX','-dpdf','-r0')
