Input:

Test and Training data from UCI HAIR

Imported and formatted to separate standard deviation from mean columns
Also separated type columns according to descriptive file features_info.txt

Output:

Tidy data with following columns:

subject - the number representing the test subject
activity - labeled activity name
domain - t representing time domain and f representing frequency domain
type - measurement type including "Body" and "Gravity"
signal - signal factors including Acc (acceleration), Gyro and Jerk
dim - directional factors for XYZ dimensions with an additional Mag for magnitude
mean/std - measurement values
