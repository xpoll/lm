package cn.blmdz.wolf.rule.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnore;

import cn.blmdz.wolf.attribute.dto.GroupedOtherAttribute;
import cn.blmdz.wolf.attribute.dto.GroupedSkuAttribute;

public abstract class BaseInput implements Serializable {
   private static final long serialVersionUID = -5808128957323868612L;
   protected List<GroupedSkuAttribute> groupedSkuAttributes;
   protected List<GroupedOtherAttribute> groupedOtherAttributes;

   @JsonIgnore
   public abstract Long getCategoryId();

   @JsonIgnore
   public abstract Long getSpuId();

   @JsonIgnore
   public abstract Set getSkuAttrKeys();

   @JsonIgnore
   public abstract List<GeneralSku> getGeneralSkus();

   public List<GroupedSkuAttribute> getGroupedSkuAttributes() {
      return this.groupedSkuAttributes;
   }

   public void setGroupedSkuAttributes(List groupedSkuAttributes) {
      this.groupedSkuAttributes = groupedSkuAttributes;
   }

   public List<GroupedOtherAttribute> getGroupedOtherAttributes() {
      return this.groupedOtherAttributes;
   }

   public void setGroupedOtherAttributes(List groupedOtherAttributes) {
      this.groupedOtherAttributes = groupedOtherAttributes;
   }
}
