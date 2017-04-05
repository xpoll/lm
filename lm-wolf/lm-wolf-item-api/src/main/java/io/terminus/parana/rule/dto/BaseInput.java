package io.terminus.parana.rule.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.List;
import java.util.Set;

public abstract class BaseInput implements Serializable {
   private static final long serialVersionUID = -5808128957323868612L;
   protected List groupedSkuAttributes;
   protected List groupedOtherAttributes;

   @JsonIgnore
   public abstract Long getCategoryId();

   @JsonIgnore
   public abstract Long getSpuId();

   @JsonIgnore
   public abstract Set getSkuAttrKeys();

   @JsonIgnore
   public abstract List getGeneralSkus();

   public List getGroupedSkuAttributes() {
      return this.groupedSkuAttributes;
   }

   public void setGroupedSkuAttributes(List groupedSkuAttributes) {
      this.groupedSkuAttributes = groupedSkuAttributes;
   }

   public List getGroupedOtherAttributes() {
      return this.groupedOtherAttributes;
   }

   public void setGroupedOtherAttributes(List groupedOtherAttributes) {
      this.groupedOtherAttributes = groupedOtherAttributes;
   }
}
