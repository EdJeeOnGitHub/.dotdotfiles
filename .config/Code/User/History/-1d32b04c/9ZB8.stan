int num_test(array[] int to_test, array[] int target_val, int test_equality) {
  int num_to_test = num_elements(to_test);
  int num_targets = num_elements(target_val);
  int result = 0;
  
  array[num_to_test] int sorted_to_test = sort_asc(to_test);
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index in 1:num_targets) {
      if (sorted_to_test[to_test_index] == target_val[target_index]) {
        if (test_equality) {
          result += 1;
        }
        
        found = 1;
        break;
      } 
    }
    
    if (!found && (1 - test_equality)) {
      result += 1;
    }
  }
  
  return(result);
}

int num_test(array[] real to_test, array[] real target_val, int test_equality) {
  int num_to_test = num_elements(to_test);
  int num_targets = num_elements(target_val);
  int result = 0;
  
  array[num_to_test] int sorted_to_test = sort_asc(to_test);
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index in 1:num_targets) {
      if (sorted_to_test[to_test_index] == target_val[target_index]) {
        if (test_equality) {
          result += 1;
        }
        
        found = 1;
        break;
      } 
    }
    
    if (!found && (1 - test_equality)) {
      result += 1;
    }
  }
  
  return(result);
}

int num_test(array[] int to_test1, array[] int to_test2, array[] int target_val1, array[] int target_val2, int test_equality) {
  int num_to_test = num_elements(to_test1);
  int num_targets1 = num_elements(target_val1);
  int num_targets2 = num_elements(target_val2);
  int result = 0;
  
  array[num_to_test] int sorted_to_test1 = sort_asc(to_test1);
  array[num_to_test] int sorted_to_test2 = sort_asc(to_test2);
  
  if (num_elements(to_test2) != num_to_test) {
    reject("to_test1 and to_test2 must have the same number of elements.");
  }
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index1 in 1:num_targets1) {
      if (sorted_to_test1[to_test_index] == target_val1[target_index1]) {
        for (target_index2 in 1:num_targets2) {
          if (sorted_to_test2[to_test_index] == target_val2[target_index2]) {
            if (test_equality) {
              result += 1;
            }
          
            found = 1;
            break;
          }
        }
        
        if (found) {
          break;
        }
      }
    }
    
    if (!found && (1 - test_equality)) {
      result += 1;
    }
  }
  
  return(result);
}

int num_equals(array[] int to_test, array[] int target_val) {
  return(num_test(to_test, target_val, 1));
}

int num_equals(array[] real to_test, array[] real target_val) {
  return(num_test(to_test, target_val, 1));
}

int num_equals(array[] int to_test1, array[] int to_test2, array[] int target_val1, array[] int target_val2) {
  return(num_test(to_test1, to_test2, target_val1, target_val2, 1));
}

array[] int count(int count_size, array[] int find_in) {
  array[count_size] int count_array = rep_array(0, count_size);
  
  for (count_index in 1:count_size) {
    count_array[count_index] = num_equals(find_in, { count_index });
  }
  
  return(count_array);
}

array[,] int count(int count_size1, int count_size2, array[] int find_in1, array[] int find_in2) {
  array[count_size1, count_size2] int count_array = rep_array(0, count_size1, count_size2);
  
  for (count_index1 in 1:count_size1) {
    for (count_index2 in 1:count_size2) {
      count_array[count_index1, count_index2] = num_equals(find_in1, find_in2, { count_index1 }, { count_index2 });
    }
  }
  
  return(count_array);
}

array[] int sort_indices_asc(array[] int group1, array[] int group2) {
  int num_indices = num_elements(group1);
  int num_groups1 = max(group1);
  int num_groups2 = max(group2);
  array[num_groups1] int group1_count = count(num_groups1, group1);
  array[num_indices] int indices = sort_indices_asc(group1);
  
  int group1_pos = 1;
  
  for (group1_index in 1:num_groups1) {
    int group1_end = group1_pos + group1_count[group1_index] - 1;
    
    indices[group1_pos:group1_end] = indices[group1_pos:group1_end][sort_indices_asc(group2[indices[group1_pos:group1_end]])];
    
    group1_pos = group1_end + 1;
  }
  
  return indices;
}

array[] int count_by_group_test(array[] int to_count, array[] int group, array[] int target_val, int test_equality) {
  int num_to_count = num_elements(to_count);
  int num_groups = max(group); // num_elements(unique(group));
  array[num_groups] int group_sizes = count(num_groups, group); 
  array[num_to_count] int to_count_group_sorted = to_count[sort_indices_asc(group)];
  
  array[num_groups] int group_count = rep_array(0, num_groups);
  int group_pos = 1;
  
  if (num_elements(group) != num_to_count) {
    reject("Incompatible array sizes.");
  }
  
  for (group_index in 1:num_groups) {
    if (group_sizes[group_index] > 0) {
      int group_end = group_pos + group_sizes[group_index] - 1;
      
      group_count[group_index] = num_test(to_count[group_pos:group_end], target_val, test_equality);
      
      group_pos = group_end + 1;
    }
  }
  
  return(group_count);
}
array[,] int count_by_group_test(array[] int to_count, array[] int group1, array[] int group2, array[] int target_val, int test_equality) {
  int num_to_count = num_elements(to_count);
  int num_groups1 = max(group1); // num_elements(unique(group));
  int num_groups2 = max(group2); 
  array[num_groups1, num_groups2] int group_sizes = count(num_groups1, num_groups2, group1, group2); 
  array[num_to_count] int to_count_group_sorted = to_count[sort_indices_asc(group1, group2)];
  
  array[num_groups1, num_groups2] int group_count = rep_array(0, num_groups1, num_groups2);
  int group_pos = 1;
  
  if (num_elements(group1) != num_to_count || num_elements(group2) != num_to_count) {
    reject("Incompatible array sizes.");
  }
  
  for (group_index1 in 1:num_groups1) {
    for (group_index2 in 1:num_groups2) {
      if (group_sizes[group_index1, group_index2] > 0) {
        int group_end = group_pos + group_sizes[group_index1, group_index2] - 1;
      
        group_count[group_index1, group_index2] = num_test(to_count[group_pos:group_end], target_val, test_equality);
        
        group_pos = group_end + 1;
      }
    }
  }
  
  return(group_count);
}

array[] int which(array[] int to_test, array[] int target_val, int test_equality) {
  int num_to_test = num_elements(to_test);
  int num_targets = num_elements(target_val);
  int num_which = num_test(to_test, target_val, test_equality);
  array[num_which] int result;
  int curr_result_index = 0;
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index in 1:num_targets) {
      if (to_test[to_test_index] == target_val[target_index]) {
        if (test_equality) {
          curr_result_index += 1;
          result[curr_result_index] = to_test_index;
        }
       
        found = 1; 
        break;
      } 
    }
        
    if (!found && (1 - test_equality)) {
      curr_result_index += 1;
      result[curr_result_index] = to_test_index;
    }
    
    if (curr_result_index >= num_which) {
      break;
    }
  }
  
  return(result);
}

array[] int which(array[] real to_test, array[] real target_val, int test_equality) {
  int num_to_test = num_elements(to_test);
  int num_targets = num_elements(target_val);
  int num_which = num_test(to_test, target_val, test_equality);
  array[num_which] int result;
  int curr_result_index = 0;
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index in 1:num_targets) {
      if (to_test[to_test_index] == target_val[target_index]) {
        if (test_equality) {
          curr_result_index += 1;
          result[curr_result_index] = to_test_index;
        }
       
        found = 1; 
        break;
      } 
    }
        
    if (!found && (1 - test_equality)) {
      curr_result_index += 1;
      result[curr_result_index] = to_test_index;
    }
    
    if (curr_result_index >= num_which) {
      break;
    }
  }
  
  return(result);
}

array[] int which_max(array[] real to_test) {
  int test_equality = 1;
  int num_to_test = num_elements(to_test);
  int num_targets = 1;
  int num_which = num_to_test; 
  array[num_which] int result;
  real target_val = max(to_test); 
  int curr_result_index = 0;
  
  for (to_test_index in 1:num_to_test) {
    int found = 0;
    
    for (target_index in 1:num_targets) {
      if (to_test[to_test_index] == target_val) {
        if (test_equality) {
          curr_result_index += 1;
          result[curr_result_index] = to_test_index;
        }
       
        found = 1; 
        break;
      } 
    }
        
    if (!found && (1 - test_equality)) {
      curr_result_index += 1;
      result[curr_result_index] = to_test_index;
    }
    
    if (curr_result_index >= num_which) {
      break;
    }
  }
  
  return(result);
}
array[] int seq(int from, int to, int by) {
  int reverse = from > to;
  int seq_len = (((1 - 2 * reverse) * (to - from)) %/% by) + 1;
  array[seq_len] int result_seq;
  
  for (seq_index in 1:seq_len) {
    result_seq[seq_index] =  from + (1 - 2 * reverse) * ((seq_index - 1) * by);
  }
  
  return(result_seq);
}

array[] int seq(int from, int to)  {
  return seq(from, to, 1);
}

array[] int seq_len(int len)  {
  return seq(1, len, 1);
}

array[] int array_subtract(array[] int left, array[] int right) {
  int array_size = num_elements(left);
  array[array_size] int array_sub;
  int right_array_size = num_elements(right);  
  
  if (right_array_size != array_size && right_array_size != 1) {
    reject("Incompatible array sizes.");
  }
  
  for (array_index in 1:array_size) {
    array_sub[array_index] = left[array_index] - right[right_array_size > 1 ? array_index : 1];
  }
  
  return(array_sub);
}

array[] int array_product(array[] int left, array[] int right) {
  int array_size = num_elements(left);
  array[array_size] int array_prod;
  int right_array_size = num_elements(right);  
  
  if (right_array_size != array_size && right_array_size != 1) {
    reject("Incompatible array sizes.");
  }
  
  for (array_index in 1:array_size) {
    array_prod[array_index] = left[array_index] * right[right_array_size > 1 ? array_index : 1];
  }
  
  return(array_prod);
}

array[] int array_add(array[] int left, array[] int right) {
  int array_size = num_elements(left);
  array[array_size] int array_sum;
  int right_array_size = num_elements(right);  
  
  if (right_array_size != array_size && right_array_size != 1) {
    reject("Incompatible array sizes.");
  }
  
  for (array_index in 1:array_size) {
    array_sum[array_index] = left[array_index] + right[right_array_size > 1 ? array_index : 1];
  }
  
  return(array_sum);
}

int in_array(int to_test, array[] int target_val) {
  int num_target = num_elements(target_val);
  
  for (target_index in 1:num_target) {
    if (to_test == target_val[target_index]) return(1);
  }
  
  return(0);
}


vector row_sum(matrix m) {
  return m * rep_vector(1, cols(m));
}

row_vector column_sum(matrix m) {
  return rep_row_vector(1, rows(m)) * m;
}

vector calc_gp_trend(array[] int dist, real alpha, real rho, vector raw) {
  int n_dist = size(dist);
  matrix[n_dist, n_dist] cov = gp_exp_quad_cov(dist, alpha, rho) + diag_matrix(rep_vector(1e-10, n_dist));
  matrix[n_dist, n_dist] L_cov = cholesky_decompose(cov);
  vector[n_dist] trend = L_cov * raw;

  return trend;
}

real normal_lb_rng(real mu, real sigma, real lb) {
  real p = normal_cdf(lb | mu, sigma);  // cdf for bounds
  real u = uniform_rng(p, 1);
  return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
}
