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
mu = [0.001,0.011];    % Machine unavailability rate caused by other causes



eta = 0.5; % corrective controls 
upsilon = 0.5; % preventive controls 
Omega = 0.5; % Compensatory Controls Implementation (Ω)
alpha = 0.5; %Deterrent Control Implementation(α)
zeta = 0.5; %detective controls
iota = (3)/( (1/Omega) + (1/alpha) + (1/zeta)) ; % Likelihood Reduction controls(ι)

L= 0.6;%cyber attack likelihood
psi= 0.6; %attack degree
delta = 0.6;%attack duration

Gamma = 3/((1/(eta))+ 1/(upsilon) + 1/(zeta)); %impact reduction controls
IRC = Gamma*100; %Impact Reduction controls

X = zeros(length(mu), (tf*100)+1, 6);
for i = 1:length(mu)
    parameters = [lambda, a,m ,beta0, mu(i),L,psi,  eta,upsilon,delta,Gamma,iota*100,IRC];
    [t,x] = Rates(parameters, x0, tf);
    X(i,:,:) = x;
   
end

    
m = figure; 

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


for i = 1:length(mu)  
    for j = 1:6  
        plot(t, X(i,:,j), 'LineWidth', 1,'LineStyle', mylinestyles(i));
        colororder(newcolors);
        hold on;     
    end
end

hold off; 


xlabel('Time units', 'FontSize', 10)
ylabel('Number of individuals', 'FontSize', 10)
%ylim([0 35])
legend({'(S) μ = 0.001', '(E) μ = 0.001', '(R) μ = 0.001', '(D) μ = 0.001', '(U) μ = 0.001', '(X) μ = 0.001', ...
        '(S) μ = 0.011', '(E) μ = 0.011', '(R) μ = 0.011', '(D) μ = 0.011', '(U) μ = 0.011', '(X) μ = 0.011'}, ...
        'Location', 'southoutside', 'NumColumns', 6, 'FontSize', 7,'Orientation','horizontal');
set(m,'Units','Inches');
pos = get(m,'Position');
set(m,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(m,'mu_test','-dpdf','-r0')

