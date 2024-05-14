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
m = [tf/8,tf]; % Parameter for lost of immunity rate (cosine's period)
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

X = zeros(length(m), (tf*100)+1, 6);
for i = 1:length(m)
    params = [lambda, a,m(i) ,beta0, mu,L,psi, eta,upsilon, delta,Gamma,iota*100,IRC];
    [t,x] = Rates(params, x0, tf);
    X(i,:,:) = x;
   
end
   
m_t = figure; 

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

for i = 1:length(m)  
    for j = 1:6  % Para cada una de las 6 columnas de x
            plot(t, X(i,:,j), 'LineWidth', 1,'LineStyle', mylinestyles(i));
            colororder(newcolors);
            hold on;     
    end
end

hold off; 


xlabel('Time units', 'FontSize', 10)
ylabel('Number of individuals', 'FontSize', 10)
ylim([0 60])
legend({'(S) m = tf/8', '(E) m = tf/8', '(R) m = tf/8', '(D) m = tf/8', '(U) m = tf/8', '(X) m = tf/8', ...
    '(S) m = tf', '(E) m = tf', '(R) m = tf', '(D) m = tf', '(U) m = tf', '(X) m = tf'}, ...
        'Location', 'southoutside', 'NumColumns', 6, 'FontSize', 8,'Orientation','horizontal');

set(m_t,'Units','Inches');
pos = get(m_t,'Position');
set(m_t,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(m_t,'m_test','-dpdf','-r0')