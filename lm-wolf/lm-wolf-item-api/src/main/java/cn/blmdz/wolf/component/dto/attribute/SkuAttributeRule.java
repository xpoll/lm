package cn.blmdz.wolf.component.dto.attribute;

import com.google.common.collect.Maps;

import cn.blmdz.wolf.attribute.dto.AttributeMetaKey;

import java.io.Serializable;
import java.util.Map;
import org.springframework.util.CollectionUtils;

public class SkuAttributeRule implements Serializable {
   private static final long serialVersionUID = -1310786503946441609L;
   private Map<AttributeMetaKey, String> attrMetas;
   private Map<String, String> attrMetasForK;
   private String attrVal;
   private String unit;
   private Boolean showImage;
   private String thumbnail;
   private String image;

   public void setAttrMetas(Map<AttributeMetaKey, String> attrMetas) {
      this.attrMetas = attrMetas;
      if(CollectionUtils.isEmpty(attrMetas)) {
         this.attrMetas = null;
      } else {
         this.attrMetasForK = Maps.newHashMapWithExpectedSize(attrMetas.size());

         for(AttributeMetaKey attributeMetaKey : attrMetas.keySet()) {
            this.attrMetasForK.put(attributeMetaKey.name(), attrMetas.get(attributeMetaKey));
         }
      }

   }

   public Map getAttrMetas() {
      return this.attrMetas;
   }

   public Map getAttrMetasForK() {
      return this.attrMetasForK;
   }

   public String getAttrVal() {
      return this.attrVal;
   }

   public void setAttrVal(String attrVal) {
      this.attrVal = attrVal;
   }

   public String getUnit() {
      return this.unit;
   }

   public void setUnit(String unit) {
      this.unit = unit;
   }

   public Boolean getShowImage() {
      return this.showImage;
   }

   public void setShowImage(Boolean showImage) {
      this.showImage = showImage;
   }

   public String getThumbnail() {
      return this.thumbnail;
   }

   public void setThumbnail(String thumbnail) {
      this.thumbnail = thumbnail;
   }

   public String getImage() {
      return this.image;
   }

   public void setImage(String image) {
      this.image = image;
   }
}
