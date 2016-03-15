figure();
confmat=importConfMat('../data/confmatkNN-2-2-sig1.5-k1-n4000.csv');
n=sum(sum(confmat));

mat=(confmat./repmat(sum(confmat,2),1,size(confmat,2)))*100;

names = cell(1,10);
for i = 1:10
    names{i} = num2str(i-1);
end


imagesc(mat);            %# Create a colored plot of the matrix values
%colormap(flipud(gray));  %# Change the colormap to gray (so higher values are
                         %#   black and lower values are white)
colormap(parula);
textStrings = num2str(mat(:),'%0.1f%%');  %# Create strings from the matrix values
textStrings = strtrim(cellstr(textStrings));  %# Remove any space padding

[x,y] = meshgrid(1:10);   %# Create x and y coordinates for the strings
hStrings = text(x(:),y(:),textStrings(:),...      %# Plot the strings
                'HorizontalAlignment','center');
midValue = mean(get(gca,'CLim'));  %# Get the middle value of the color range
textColors = repmat(mat(:) < midValue,1,3);  %# Choose white or black for the
                                             %#   text color of the strings so
                                             %#   they can be easily seen over
                                             %#   the background color
set(hStrings,{'Color'},num2cell(textColors,2));  %# Change the text colors

colorbar;
caxis([0 100]);

set(gca,'XTickLabel',names);
set(gca,'YTickLabel',names);
xlabel('Reference');
ylabel('Prediction');
title('k-NN Confusion Matrix, Group 2, Member 2, 100 DPI, N=4000');

%%
clear all;
figure();
confmat=importConfMat('../data/confmatkNNMultiAll-dpi100-sig1.5-k1-n40000.csv');
n=sum(sum(confmat));

mat=(confmat./repmat(sum(confmat,2),1,size(confmat,2)))*100;

names = cell(1,10);
for i = 1:10
    names{i} = num2str(i-1);
end


imagesc(mat);            %# Create a colored plot of the matrix values
%colormap(flipud(gray));  %# Change the colormap to gray (so higher values are
                         %#   black and lower values are white)
colormap(parula);
textStrings = num2str(mat(:),'%0.1f%%');  %# Create strings from the matrix values
textStrings = strtrim(cellstr(textStrings));  %# Remove any space padding

[x,y] = meshgrid(1:10);   %# Create x and y coordinates for the strings
hStrings = text(x(:),y(:),textStrings(:),...      %# Plot the strings
                'HorizontalAlignment','center');
midValue = mean(get(gca,'CLim'));  %# Get the middle value of the color range
textColors = repmat(mat(:) < midValue,1,3);  %# Choose white or black for the
                                             %#   text color of the strings so
                                             %#   they can be easily seen over
                                             %#   the background color
set(hStrings,{'Color'},num2cell(textColors,2));  %# Change the text colors

colorbar;
caxis([0 100]);

set(gca,'XTickLabel',names);
set(gca,'YTickLabel',names);
xlabel('Reference');
ylabel('Prediction');
title('k-NN Confusion Matrix, multiple persons, 100 DPI, N=40000');

%%
clear all;
figure();
confmat=importConfMat('../data/confmatkNNMultiLOO-dpi100-sig1.5-k1-n40000.csv');
n=sum(sum(confmat));

mat=(confmat./repmat(sum(confmat,2),1,size(confmat,2)))*100;

names = cell(1,10);
for i = 1:10
    names{i} = num2str(i-1);
end


imagesc(mat);            %# Create a colored plot of the matrix values
%colormap(flipud(gray));  %# Change the colormap to gray (so higher values are
                         %#   black and lower values are white)
colormap(parula);
textStrings = num2str(mat(:),'%0.1f%%');  %# Create strings from the matrix values
textStrings = strtrim(cellstr(textStrings));  %# Remove any space padding

[x,y] = meshgrid(1:10);   %# Create x and y coordinates for the strings
hStrings = text(x(:),y(:),textStrings(:),...      %# Plot the strings
                'HorizontalAlignment','center');
midValue = mean(get(gca,'CLim'));  %# Get the middle value of the color range
textColors = repmat(mat(:) < midValue,1,3);  %# Choose white or black for the
                                             %#   text color of the strings so
                                             %#   they can be easily seen over
                                             %#   the background color
set(hStrings,{'Color'},num2cell(textColors,2));  %# Change the text colors

colorbar;
caxis([0 100]);

set(gca,'XTickLabel',names);
set(gca,'YTickLabel',names);
xlabel('Reference');
ylabel('Prediction');
title('k-NN Confusion Matrix, multiple persons, LOO, 100 DPI, N=40000');

