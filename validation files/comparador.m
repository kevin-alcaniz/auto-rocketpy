%%% SCRIPT PARA COMPARAR EL SIMULADOR ORIGINAL DE ROCKETPY CON
%%% LOS RESULTADOS DE AUTO-ROCKETPY

clear; clc; close all;

% Se leen los archivos de resultados
original = importfile_cd('original_flight_data.csv',[2 Inf]);
drag_test = importfile_cd('auto_drag_test_2.csv',[2 Inf]);
matlab_test = importfile_cd('matlab_test.csv',[2 Inf]);
coeff = importfile_coeff('coefficients_flight_data.csv',[2 Inf]);
coeff_test = importfile_coeff('auto_coefficients_test.csv',[2 Inf]);

%% Plots drag
figure(1);
plot(original.t,original.z,'LineWidth',2,'Color','k')
hold on
% plot(drag_test.t,drag_test.R3,'LineWidth',2,'Color','r')
% grid on
xlabel 'Tiempo (s)'
% legend('Original','Drag Test')

% Plots coeff
% figure(2);
plot(coeff.t,coeff.z,'LineWidth',2,'Color','r')
hold on
% plot(coeff_test.t,coeff_test.z,'LineWidth',2,'Color','b')
% grid on
% xlabel 'Tiempo (s)'
legend('Original','Cofficients')