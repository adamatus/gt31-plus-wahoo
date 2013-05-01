gt31-plus-wahoo
===============

Simple script to combine the accurate GPS data from a [Locosys
GT-31](http://www.locosystech.com/product.php?id=30) and the various data
recorded simultaneously with a mobile phone (which may include less-acurate GPS,
heart-rate, stride, bike speed, bike cadence, bike power, etc).

Currently only supports data collected with the [Wahoo Fitness App on iPhone](https://itunes.apple.com/us/app/fisica-fitness/id391599899?mt=8), but
other data collection apps can be added if example output files are provided
to me. Additionally, I only have HR and bike cadence speed sensors, so power and
stride are implented provisionally and may not work.

# Usage

    >> combine.with.gt31('GT31_FILE_NAME','WAHOO_FILE_WF.pwx')

