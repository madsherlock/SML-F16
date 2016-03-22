%% Preprocessing script (crop & downsample with ImageMagick).
% Crops images based on Corners.txt supplied with dataset,
% and downsamples to 200 and 100 DPI.
% The skewed, scanned 300 DPI images are first
% rotated and scaled to 1200x1200 pixels using the four point Perspective
% transform (for each digit), which preserves straight lines. The four
% points are the corners of the 20 by 20 digit bounding box, as specified
% in Corners.txt. Then the 1200x1200 DPI image is downsampled to lower DPI.
% In summary, each 300 DPI page (with two digits) is converted into six
% images (three pixel densities per digit, two digits per page).
% The reasoning behind this method is that the grid into which digits
% have been written will be positioned at (or near) rows and columns which
% are multiples of 60*DPI/300. This, of course, requires accurate
% Corners.txt specification.
%
% Copyright 2016 Mikael Westermann
% 
% Licensed under the ImageMagick License (the "License"); you may not use
% this file except in compliance with the License.  You may obtain a copy
% of the License at
%
%   http://www.imagemagick.org/script/license.php
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations
% under the License.
   
databasePath='../../SML-database/'; %Input directory (contains 2016 folder)
cropPath='../data/cropped_images/'; %Output directory (must exist)

%OK persons.
persons = [...
    2016,1,1;
    2016,1,2;
    2016,2,1;
    2016,2,2;
    2016,3,1;
    2016,3,2;
    2016,4,1;
    2016,4,2;
    2016,4,3;
    2016,5,1;
    2016,5,2;
    2016,5,3;
    2016,6,1;
    2016,7,1;
    2016,8,1;
    2016,10,1;
    2016,10,2;
    2016,11,1;
    2016,11,2;
    2016,11,3;
    2016,13,1;
    2016,14,1;
    2016,14,2;
    ];
for personIdx=1:length(persons)
    year=persons(personIdx,1);
    group=persons(personIdx,2);
    member=persons(personIdx,3);
    display(sprintf('Y%d G%d M%d...',year,group,member));
    %sprintf([databasePath '%d/group%d/member%d/Corners.txt'],...
    %    year,group,member)
    corners=csvread(...
        sprintf([databasePath '%d/group%d/member%d/Corners.txt'],...
        year,group,member),1);
    corners=corners(:,1:8);
    for p=0:4
        for digit=[2*p 2*p+1]
            %Crop
            system(...
                sprintf(['convert '...
                    databasePath...
                    '%d/group%d/member%d/'...
                    'Ciphers300-%d.png -matte -virtual-pixel black '...
                    '-distort Perspective '''...
                    '%d,%d 0,0 %d,%d 1200,0 %d,%d 0,1200 %d,%d 1200,1200'' '...
                    '-crop 1200x1200+0+0 '...
                    cropPath...
                    'cropY%dG%dM%d-300-%d.png'],...
                    year,group,member,...
                    p,...
                    corners(digit+1,1:8),...
                    year,group,member,...
                    digit)...
            );
            %Downsample
            for dpi=[100,200]
                system(...
                    sprintf(['convert -density 300 '...
                        '-units PixelsPerInch '...
                        cropPath...
                        'cropY%dG%dM%d-300-%d.png '...
                        '-resample %d '...
                        cropPath...
                        'cropY%dG%dM%d-%d-%d.png'],...
                        year,group,member,digit,...
                        dpi,...
                        year,group,member,dpi,digit)...
                );
            end
        end
    end
end