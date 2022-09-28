nuts2_hma <- data.frame(gss_code = c('E09000023', 'E09000028', 'E09000022', 'E09000004', 'E09000011', 'E09000002', 'E09000016', 'E09000026',
                               'E09000031', 'E09000010', 'E09000006', 'E09000008', 'E09000021', 'E09000024', 'E09000029', 'E09000003',
                               'E09000005', 'E09000009', 'E09000015', 'E09000017', 'E09000018', 'E09000027', 'E09000001', 'E09000007',
                               'E09000033', 'E09000013', 'E09000020', 'E09000032', 'E09000012', 'E09000025', 'E09000030', 'E09000014',
                               'E09000019'),
                  constraint_area = c('Inner London - East', 'Inner London - East', 'Inner London - East',
                                      'Outer London - East and North East', 'Outer London - East and North East',
                                      'Outer London - East and North East', 'Outer London - East and North East',
                                      'Outer London - East and North East', 'Outer London - East and North East',
                                      'Outer London - East and North East', 'Outer London - South', 'Outer London - South',
                                      'Outer London - South', 'Outer London - South', 'Outer London - South',
                                      'Outer London - West and North West', 'Outer London - West and North West',
                                      'Outer London - West and North West', 'Outer London - West and North West',
                                      'Outer London - West and North West', 'Outer London - West and North West',
                                      'Outer London - West and North West', 'Inner London - West', 'Inner London - West',
                                      'Inner London - West', 'Inner London - West', 'Inner London - West',
                                      'Inner London - West', 'Inner London - East', 'Inner London - East',
                                      'Inner London - East', 'Inner London - East', 'Inner London - East'))

saveRDS(nuts2_hma, "input_data/flexible_area_model/lookups/NUTS2_hma.rds")

rm(list=ls())
