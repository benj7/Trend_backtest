dyCSScool <- function(dygraph){
    
    dygraph$x$css <- '
    
  .dygraph-title {
  color: navy;
  font-weight: bold;
  }
  
  .dygraph-axis-label {
  font-size: 11px;
  }
  
  .background-color{
  font-size: 11px;
  }

  .dygraph-legend {
  width: auto !important;
  min-width: 150px;
  color: white;
  background-color: #BABABA !important;
  padding-left:5px;
  border-color:#BABABA;
  border-style:solid;
  border-width:thin;
  transition:0s 4s;
  z-index: 80 !important;
  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  border-radius: 3px;
  }
  
  .dygraph-legend:hover{
  transform: translate(-110%);
  transition: 0s;
  }
  
  .dygraph-legend > span {
  color: black;
  padding-left:5px;
  padding-right:2px;
  margin-left:-5px;
  background-color: white !important;
  display: block;
  }
  
  .dygraph-legend > span:first-child {
  margin-top:2px;
  }
  
  .dygraph-legend > span > span{
  display: inline;
  }
  
  .highlight {
  border-left: 2px solid #BABABA;
  padding-left:3px !important;
  }
  
  
  '
    dygraph
}


