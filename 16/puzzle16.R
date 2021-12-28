################################################################################
##
## == Advent of Code 2021 ==
## Day: 16
## Author: Jakob Dambon
##
################################################################################


library(BMS)


BITS <- function(bin, set_up = NULL) {
  vers <- strtoi(paste0(bin[1:3], collapse = ""), base = 2L)
  type <- strtoi(paste0(bin[4:6], collapse = ""), base = 2L)
  len_subpack <- NULL
  literal_val <- NULL
  end_pos <- NULL
  
  if (type == 4) {
    # literal value
    cont <- TRUE
    i <- 7
    sub_bits <- c()
    while(cont) {
      if (bin[i] == 0) 
        cont <- FALSE
      sub_bits <- c(sub_bits, bin[i+1:4])
      i <- i + 5
    }
    end_pos <- i-1
    literal_val <- sum(sub_bits*2^((length(sub_bits)-1):0))
  } else {
    # operator
    if (bin[7] == "0") {
      len_subpack <- strtoi(paste0(bin[7 + 1:15], collapse = ""), base = 2L)
      i_subpack <- j <- 0
      while(j < len_subpack) {
        subpack <- BITS(bin[(7 + 15 + j) + 1:(len_subpack-j)])
        j <- j+subpack$end_pos
        i_subpack <- i_subpack+1
        set_up[[i_subpack]] <- subpack
      }
      end_pos <- 22+len_subpack
      
    } else {
      len_subpack <- strtoi(paste0(bin[7 + 1:11], collapse = ""), base = 2L)
      i_subpack <- j <- 0
      while(i_subpack < len_subpack) {
        subpack <- BITS(bin[(7 + 11 + j + 1):length(bin)])
        j <- j+subpack$end_pos
        i_subpack <- i_subpack+1
        set_up[[i_subpack]] <- subpack
      }
      end_pos <- 18+j
    }
    
  }
  return(list(
    version = vers, 
    type_ID = type, 
    len_subpack = len_subpack, 
    literal_val = literal_val,
    end_pos = end_pos,
    set_up = set_up
  ))
}

version_sum <- function(ll) {
  if (is.null(ll$set_up)) {
    ll$version
  } else {
    ll$version + sum(sapply(ll$set_up, version_sum))
  }
}



calc_packets <- function(ll) {
  if (is.null(ll$set_up)) {
    print(paste0("LV: ", ll$literal_val))
    ll$literal_val
  } else {
    sub_vals <- sapply(ll$set_up, calc_packets)
    fn <- switch(
      as.character(ll$type_ID), 
      "0" = sum,
      "1" = prod,
      "2" = min,
      "3" = max,
      "5" = function(x) {(as.numeric(x[1] > x[2]))},
      "6" = function(x) {(as.numeric(x[1] < x[2]))},
      "7" = function(x) {(as.numeric(x[1] == x[2]))}
    )
    print(paste0("OV: ", fn(sub_vals)))
    
    fn(sub_vals)
  
  }
}

bin <- hex2bin("9C0141080250320F1802104A08")
out <- BITS(bin)

version_sum(out)
calc_packets(out)

bin_final <- hex2bin("E058F79802FA00A4C1C496E5C738D860094BDF5F3ED004277DD87BB36C8EA800BDC3891D4AFA212012B64FE21801AB80021712E3CC771006A3E47B8811E4C01900043A1D41686E200DC4B8DB06C001098411C22B30085B2D6B743A6277CF719B28C9EA11AEABB6D200C9E6C6F801F493C7FE13278FFC26467C869BC802839E489C19934D935C984B88460085002F931F7D978740668A8C0139279C00D40401E8D1082318002111CE0F460500BE462F3350CD20AF339A7BB4599DA7B755B9E6B6007D25E87F3D2977543F00016A2DCB029009193D6842A754015CCAF652D6609D2F1EE27B28200C0A4B1DFCC9AC0109F82C4FC17880485E00D4C0010F8D110E118803F0DA1845A932B82E200D41E94AD7977699FED38C0169DD53B986BEE7E00A49A2CE554A73D5A6ED2F64B4804419508B00584019877142180803715224C613009E795E58FA45EA7C04C012D004E7E3FE64C27E3FE64C24FA5D331CFB024E0064DEEB49D0CC401A2004363AC6C8344008641B8351B08010882917E3D1801D2C7CA0124AE32DD3DDE86CF52BBFAAC2420099AC01496269FD65FA583A5A9ECD781A20094CE10A73F5F4EB450200D326D270021A9F8A349F7F897E85A4020CF802F238AEAA8D22D1397BF27A97FD220898600C4926CBAFCD1180087738FD353ECB7FDE94A6FBCAA0C3794875708032D8A1A0084AE378B994AE378B9A8007CD370A6F36C17C9BFCAEF18A73B2028C0A004CBC7D695773FAF1006E52539D2CFD800D24B577E1398C259802D3D23AB00540010A8611260D0002130D23645D3004A6791F22D802931FA4E46B31FA4E4686004A8014805AE0801AC050C38010600580109EC03CC200DD40031F100B166005200898A00690061860072801CE007B001573B5493004248EA553E462EC401A64EE2F6C7E23740094C952AFF031401A95A7192475CACF5E3F988E29627600E724DBA14CBE710C2C4E72302C91D12B0063F2BBFFC6A586A763B89C4DC9A0")
out <- BITS(bin_final)

version_sum(out)
val <- calc_packets(out)

