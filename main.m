Ao = [0 .7 .7; .8 0 .2; .15 .9 0];
dvo = [100 125 75];

[flow_matrix, demand_flow, sum_demand, A, b] = determine_flow(Ao, dvo);