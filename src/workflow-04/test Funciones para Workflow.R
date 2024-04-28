require("data.table")
require("ParamHelpers")



param_local <- list()


parametrizar  <- function( lparam )
{
  param_fijos  <- copy( lparam )
  hs  <- list()
  
  for( param  in  names( lparam ) )
  {
    if( length( lparam[[ param ]] ) > 1 )
    {
      desde  <- as.numeric( lparam[[ param ]][[1]]  )
      hasta  <- as.numeric( lparam[[ param ]][[2]]  )
      
      if( length( lparam[[ param ]] ) == 2 )
      {
        hs  <- append( hs,  
                       list( makeNumericParam( param, lower= desde, upper= hasta)  ) )
      } else {
        hs  <- append( hs, 
                       list( makeIntegerParam( param, lower= desde, upper= hasta) ) )
      }
      
      param_fijos[[ param ]] <- NULL  #lo quito 
    }
  }
  
  return( list( "param_fijos" =  param_fijos,
                "paramSet"    =  hs ) )
}




param_local$lgb_param <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = FALSE,
  # White Gloves Bayesian Optimization, with a happy narrow exploration
  learning_rate = c( 0.02, 0.8 ),
  feature_fraction = c( 0.5, 0.9 ),
  num_leaves = c( 300L, 1024L,  "integer" ),
  min_data_in_leaf = c( 100L, 2000L, "integer" )
)


apertura  <- parametrizar( param_local$lgb_param )
PARAM$lgb_basicos <- apertura$param_fijos