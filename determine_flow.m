function [change_vector, flow_demand, sum_demand A, b] = determine_flow (naive_demand_matrix, demand_vector)

% Demand matrix is row-stochastic
% Demand vector is the number of boardings per station

n = length(demand_vector);
n2 = n*n;
demand_matrix = naive_demand_matrix;
flow_demand = [];
sum_demand = zeros(1,n);

for i=1:n
    for j=1:n
        sum_demand(i) = sum_demand(i) + naive_demand_matrix(i,j);
    end
end

for i=1:n
    for j=1:n
        demand_matrix(i,j) = demand_matrix(i,j) / sum_demand(i);
        flow_demand = [flow_demand demand_matrix(i,j)*demand_vector(i)];
    end
end

A = [];
b = [];

% Constraint 1: make sure that all changes are greater than zero
for i=1:n2
    temp = zeros(1, n2);
    temp(i) = -1;
    A = [A; temp];
    b = [b 0];
end

% Determine which ones to change
% Determine which ones have direction-constant changes
% Determine which ones have 


% Constraints 2 and 3 need to be adjusted for each iteration

% Constraint 2: make sure that at the end, the demand from A to B equals
% that of B to A.
for i=1:n
    for j=1:n
        temp = zeros(1, n2);
        temp((i-1)*n + j) = flow_demand((i-1)*n + j);
        temp((j-1)*n + i) = (-1)*flow_demand((j-1)*n + i);
        A = [A; temp];
        b = [b 0];
    end
end

% Constraint 3: make sure that at the end the flow to and from each
% location is known.
for i=1:n
    for j=1:n
        temp = zeros(1, n2);
        temp((i-1)*n + j) = flow_demand((i-1)*n + j);
    end
    A = [A; temp];
    b = [b demand_vector(i)];
    A = [A; temp];
    b = [b (-1)*demand_vector(i)];
end
% still constraint 3
for j=1:n
    for i=1:n
        temp = zeros(1, n2);
        temp((i-1)*n + j) = flow_demand((i-1)*n + j);
    end
    A = [A; temp];
    b = [b demand_vector(j)];
    A = [A; temp];
    b = [b (-1)*demand_vector(j)];
end

change_vector = linprog(flow_demand, A, b);

% flow_matrix = [];
% for i=1:n
%     temp = [];
%     for j=1:n
%         temp = [temp change_vector((i-1)*n+j) * flow_demand((i-1)*n + j)];
%     end
%     flow_matrix = [flow_matrix; temp];
% end

end