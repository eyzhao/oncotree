package org.mskcc.oncotree.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.mskcc.oncotree.model.Links;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-04T17:16:11.368Z")
public class ChildrenListRespData  {
  
  private Links links = null;
  private Integer id = null;

  
  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("links")
  public Links getLinks() {
    return links;
  }
  public void setLinks(Links links) {
    this.links = links;
  }

  
  /**
   * Child ID
   **/
  @ApiModelProperty(value = "Child ID")
  @JsonProperty("id")
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
    this.id = id;
  }

  

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ChildrenListRespData childrenListRespData = (ChildrenListRespData) o;
    return Objects.equals(links, childrenListRespData.links) &&
        Objects.equals(id, childrenListRespData.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(links, id);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ChildrenListRespData {\n");
    
    sb.append("  links: ").append(links).append("\n");
    sb.append("  id: ").append(id).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
