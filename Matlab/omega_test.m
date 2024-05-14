%%Initials Conditions 
x0 = [60; % Susceptible (S)
      9;   % Exposed (E)
      0;  % Recover (R)
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


eta = 0.5; % corrective controls 
upsilon = 0.5; % preventive controls 
Omega = [0.25,1]; % Compensatory Controls Implementation (Ω)
alpha = 0.5; %Deterrent Control Implementation(α)

zeta = 0.5; %detective controls


L= 0.6;%cyber attack likelihood
psi= 0.6; %attack degree
delta = 0.6;%attack duration
iota = [3/((1/(Omega(1)))+ 1/(alpha) + 1/(zeta)),3/((1/(Omega(2)))+ 1/(alpha) + 1/(zeta))]; % Likelihood Reduction controls(ι)
Gamma = 3/((1/(eta))+ 1/(upsilon) + 1/(zeta));%impact reduction controls (Γ)
IRC = Gamma*100; %Impact Reduction controls:
X = zeros(length(Omega), (tf*100)+1, 6);%array to store the solutions of the simulations
for i = 1:length(Omega)
    params = [lambda, a,m ,beta0, mu,L,psi, eta,upsilon, delta,Gamma,iota(i)*100,IRC];
    [t,x] = Rates(params, x0, tf);
    X(i,:,:) = x;
   
end

    
o = figure; 

% Colors and Line styles for each analysis
mylinestyles = ["-", "--","-.","-", "--", ":"];

newcolors = [
    0 0.3 1;%Blue
    221/255 130/255 75/255;% Orange
    0.3921 0.835 0.09; %green
    0.47 0.17 0.5294; % purple
    
    1 0 0; %red
    
    0 0 0; %black
];



for i = 1:length(Omega)  
    for j = 1:6  
            plot(t, X(i,:,j), 'LineWidth', 1,'LineStyle', mylinestyles(i));
            colororder(newcolors);
            hold on;     
    end
end

hold off; 



xlabel('Time units', 'FontSize', 10)
ylabel('Number of individuals', 'FontSize', 10)

legend({'(S) Ω = 0.1', '(E) Ω = 0.1', '(R) Ω = 0.1', '(D) Ω = 0.1', '(U) Ω = 0.1', '(X) Ω = 0.1', ...
        '(S) Ω = 0.9', '(E) Ω = 0.9', '(R) Ω = 0.9', '(D) Ω = 0.9', '(U) Ω = 0.9', '(X) Ω = 0.9'}, ...
        'Location', 'southoutside', 'NumColumns', 6, 'FontSize', 9,'Orientation','horizontal');

set(o,'Units','Inches');
pos = get(o,'Position');
set(o,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(o,'omega_test','-dpdf','-r0')