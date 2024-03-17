box::use(
  testthat[expect_equal, test_that],
)
box::use(
  app/logic/blackmarble_utils[julian_to_month,
                              remove_fill_value_from_satellite_data,
                              apply_scaling_factor_to_viirs_data
                              ]
)

# Define the test cases
test_that("julian_to_month returns correct month for given Julian dates", {
  expect_equal(julian_to_month("001"), "01")
  expect_equal(julian_to_month("032"), "02")
  expect_equal(julian_to_month("060"), "03")
  expect_equal(julian_to_month("091"), "04")
  expect_equal(julian_to_month("121"), "05")
  expect_equal(julian_to_month("152"), "06")
  expect_equal(julian_to_month("182"), "07")
  expect_equal(julian_to_month("213"), "08")
  expect_equal(julian_to_month("244"), "09")
  expect_equal(julian_to_month("274"), "10")
  expect_equal(julian_to_month("305"), "11")
  expect_equal(julian_to_month("335"), "12")
})



# Define the unit test
test_that("remove_fill_value_from_satellite_data correctly removes artifact values", {
  # Create a sample dataset for testing
  # For simplicity, let's assume a 3x3 matrix with some artifact values
  sample_data <- matrix(c(255, -999.9, -32768,
                          100, 255, 65535,
                          -999.9, -32768, 255), nrow = 3, byrow = TRUE)

  # Test for removal of artifact value 255
  cleaned_data_255 <- remove_fill_value_from_satellite_data(sample_data, "Granule")
  expect_equal(sum(is.na(cleaned_data_255)), 1,
               info =     "Expected 1 NA value after removing artifact value 255")

  # Test for removal of artifact value -999.9
  cleaned_data_999 <- remove_fill_value_from_satellite_data(sample_data, "UTC_Time")
  expect_equal(sum(is.na(cleaned_data_999)), 1,
               info =     "Expected 1 NA value after removing artifact value -999.9")

  # Test for removal of artifact value -32768
  cleaned_data_32768 <- remove_fill_value_from_satellite_data(sample_data, "Sensor_Azimuth")

  expect_equal(sum(is.na(cleaned_data_32768)), 1,
              info =  "Expected 1 NA value after removing artifact value -32768")

  # Test for removal of artifact value 65535
  cleaned_data_65535 <- remove_fill_value_from_satellite_data(sample_data,
                         "BrightnessTemperature_M12")

  expect_equal(sum(is.na(cleaned_data_65535)), 1,
               info =   "Expected 1 NA value after removing artifact value 65535")
})


# Define the unit test
test_that("apply_scaling_factor_to_viirs_data correctly applies scaling factor", {
  # Create a sample dataset for testing
  # For simplicity, let's assume a 3x3 matrix with some VIIRS data
  sample_data <- matrix(c(25.5, -99.99, -3276.8,
                          10, 25.5, 6553.5,
                          -99.99, -3276.8, 25.5), nrow = 3, byrow = TRUE)

  # Test for scaling factor applied to DNB_At_Sensor_Radiance variable
  scaled_data <- apply_scaling_factor_to_viirs_data(sample_data, "DNB_At_Sensor_Radiance")
  expect_equal(scaled_data[1, 1], 2.55,
               "Expected scaled value for DNB_At_Sensor_Radiance to be 2.55")

  # Test for scaling factor applied to DNB_BRDF-Corrected_NTL variable
  scaled_data <- apply_scaling_factor_to_viirs_data(sample_data, "DNB_BRDF-Corrected_NTL")
  expect_equal(scaled_data[2, 2], 2.55,
               "Expected scaled value for DNB_BRDF-Corrected_NTL to be 2.55")

  # Test for scaling factor applied to AllAngle_Composite_Snow_Covered variable
  scaled_data <- apply_scaling_factor_to_viirs_data(sample_data, "AllAngle_Composite_Snow_Covered")
  expect_equal(scaled_data[3, 3], 2.55,
               "Expected scaled value for AllAngle_Composite_Snow_Covered to be 2.55")
})

