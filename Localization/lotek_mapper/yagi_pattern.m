%% svn: $Id: yagi_pattern.m 10 2010-01-14 04:45:11Z john $
%% 
%% theoretical one-way E-field gain patterns of yagi antennas
%% used with lotek transmitter tags
%%
%% Outputs files of gain values, at a grid of (theta, phi) values.  
%% Each row of file is at constant theta, changing phi.
%% Theta: from 0 to 360 in steps of dth.
%% Phi: from 0 to 90 in steps of dphi.
%% Slightly modified from Orfanidis' 6 element Example 22.5.3
%%
%% Ref: Sophocles Orfanidis (2008).  Electromagnetic Waves and Antennas
%%      Online textbook.  Available at:
%%      http://www.ece.rutgers.edu/~orfanidi/ewa/
%%
%% FIXME: get real antenna spacing, element lengths, and diameter measurements
%%        for both antennas!

addpath("Orfanidis_ewa");

%% Note: units for L, a, d are wavelengths.

%%% 5 element Yagi

L = [0.476, 0.452, 0.436, 0.430, 0.434];    % lengths of Reflector, driven, and director elements
a = 0.003369 * [1,1,1,1,1];                 % diameters of wires in same elements
d = [-0.25, 0, 0.289, 0.695, 1.018];	    % spacing of elements along boom

[I,D,Rfb] = yagi(L,a,d);		    % get current patterns

dth  = 3;			            % step size for theta, in degrees
dphi = 3;				    % step size for phi, in degrees

gm = zeros(1 + 360 / dth, 0);		    % initial empty gain matrix

for phi=0:dphi:90                           % loop over phi
  [ge,gh,th] = gain2sg(L,d,I,360/dth,phi);  % get gain with global normalization
  gm = [gm, ge'];                           % add as column to gain the matrix
  %% dbp2(th,ge,30,40);                     % to visualize as generated
endfor

sgm = size(gm);
save -ascii yagi_5_pattern.txt sgm gm;	    % save to text file

%%% 9 element Yagi  - NOT USED BY MAPPER.R CODE SO FAR

L = [0.482, 0.452, 0.432, 0.415, 0.407, 0.398, 0.390, 0.390, 0.390];    % lengths of Reflector, driven, and director elements
a = 0.0085 * [1,1,1,1,1,1,1,1,1];                    % diameters of wires in same elements
d = [-0.2, 0, .2, .4, .6, .8, 1.0, 1.2, 1.4];	    % spacing of elements along boom

[I,D,Rfb] = yagi(L,a,d);		    % get current patterns

dth  = 3;			            % step size for theta, in degrees
dphi = 3;				    % step size for phi, in degrees

gm = zeros(1 + 360 / dth, 0);		    % initial empty gain matrix

for phi=0:dphi:90                           % loop over phi
  [ge,gh,th] = gain2sg(L,d,I,360/dth,phi);  % get gain with global normalization
  gm = [gm, ge'];                           % add as column to gain the matrix
  %% dbp2(th,ge,30,40);                     % to visualize as generated
endfor

sgm = size(gm);
save -ascii yagi_9_pattern.txt sgm gm;	    % save to text file


