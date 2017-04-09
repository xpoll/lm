package cn.blmdz.wolf.item.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;

public class ItemDetail implements Serializable {
   private static final long serialVersionUID = 1837956092030029010L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long itemId;
   private List images;
   @JsonIgnore
   private String imagesJson;
   private Map packing;
   @JsonIgnore
   private String packingJson;
   private String service;
   private String detail;
   private Date createdAt;
   private Date updatedAt;

   public void setImages(List images) {
      this.images = images;
      if(images == null) {
         this.imagesJson = null;
      } else {
         this.imagesJson = JsonMapper.JSON_NON_EMPTY_MAPPER.toJson(images);
      }

   }

   public void setImagesJson(String imagesJson) throws Exception {
      this.imagesJson = imagesJson;
      if(Strings.isNullOrEmpty(imagesJson)) {
         this.images = Collections.emptyList();
      } else {
         this.images = (List)objectMapper.readValue(imagesJson, new TypeReference() {
         });
      }

   }

   public void setPacking(Map packing) {
      this.packing = packing;
      if(this.images == null) {
         this.packingJson = null;
      } else {
         try {
            this.packingJson = objectMapper.writeValueAsString(packing);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setPackingJson(String packingJson) throws Exception {
      this.packingJson = packingJson;
      if(Strings.isNullOrEmpty(packingJson)) {
         this.packing = Collections.emptyMap();
      } else {
         this.packing = (Map)objectMapper.readValue(packingJson, JacksonType.MAP_OF_STRING);
      }

   }

   public String toString() {
      return "ItemDetail(itemId=" + this.getItemId() + ", images=" + this.getImages() + ", imagesJson=" + this.getImagesJson() + ", packing=" + this.getPacking() + ", packingJson=" + this.getPackingJson() + ", service=" + this.getService() + ", detail=" + this.getDetail() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ItemDetail)) {
         return false;
      } else {
         ItemDetail other = (ItemDetail)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$itemId = this.getItemId();
            Object other$itemId = other.getItemId();
            if(this$itemId == null) {
               if(other$itemId != null) {
                  return false;
               }
            } else if(!this$itemId.equals(other$itemId)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ItemDetail;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $itemId = this.getItemId();
      result = result * 59 + ($itemId == null?0:$itemId.hashCode());
      return result;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public List getImages() {
      return this.images;
   }

   public String getImagesJson() {
      return this.imagesJson;
   }

   public Map getPacking() {
      return this.packing;
   }

   public String getPackingJson() {
      return this.packingJson;
   }

   public String getService() {
      return this.service;
   }

   public void setService(String service) {
      this.service = service;
   }

   public String getDetail() {
      return this.detail;
   }

   public void setDetail(String detail) {
      this.detail = detail;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
