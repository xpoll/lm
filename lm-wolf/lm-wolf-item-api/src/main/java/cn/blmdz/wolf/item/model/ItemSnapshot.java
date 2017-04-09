package cn.blmdz.wolf.item.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;

public class ItemSnapshot implements Serializable {
   private static final long serialVersionUID = 8020192989436775091L;
   private static final ObjectMapper objectMapper = JsonMapper.nonDefaultMapper().getMapper();
   private Long id;
   private Long itemId;
   private String itemCode;
   private String itemInfoMd5;
   private Long shopId;
   private String shopName;
   private String name;
   private String mainImage;
   private List images;
   @JsonIgnore
   private String imagesJson;
   private String advertise;
   private String specification;
   @JsonIgnore
   private String extraJson;
   private Map extra;
   private Map tags;
   private String tagsJson;
   private List skuAttrs;
   private List otherAttrs;
   @JsonIgnore
   private String otherAttrsJson;
   @JsonIgnore
   private String skuAttrsJson;
   private String detail;
   private Date createdAt;

   public void setExtraJson(String extraJson) throws Exception {
      this.extraJson = extraJson;
      if(Strings.isNullOrEmpty(extraJson)) {
         this.extra = Collections.emptyMap();
      } else {
         this.extra = (Map)objectMapper.readValue(extraJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setExtra(Map extra) {
      this.extra = extra;
      if(extra != null && !extra.isEmpty()) {
         try {
            this.extraJson = objectMapper.writeValueAsString(extra);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.extraJson = null;
      }

   }

   public void setTagsJson(String tagsJson) throws Exception {
      this.tagsJson = tagsJson;
      if(Strings.isNullOrEmpty(tagsJson)) {
         this.tags = Collections.emptyMap();
      } else {
         this.tags = (Map)objectMapper.readValue(tagsJson, JacksonType.MAP_OF_STRING);
      }

   }

   public void setTags(Map tags) {
      this.tags = tags;
      if(tags != null && !tags.isEmpty()) {
         try {
            this.tagsJson = objectMapper.writeValueAsString(tags);
         } catch (Exception var3) {
            ;
         }
      } else {
         this.tagsJson = null;
      }

   }

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

   public void setSkuAttrs(List skuAttrs) {
      this.skuAttrs = skuAttrs;
      if(CollectionUtils.isEmpty(skuAttrs)) {
         this.skuAttrsJson = null;
      } else {
         try {
            this.skuAttrsJson = objectMapper.writeValueAsString(skuAttrs);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setOtherAttrs(List otherAttrs) {
      this.otherAttrs = otherAttrs;
      if(otherAttrs == null) {
         this.otherAttrsJson = null;
      } else {
         try {
            this.otherAttrsJson = objectMapper.writeValueAsString(otherAttrs);
         } catch (Exception var3) {
            ;
         }
      }

   }

   public void setSkuAttrsJson(String skuAttrsJson) throws Exception {
      this.skuAttrsJson = skuAttrsJson;
      if(Strings.isNullOrEmpty(skuAttrsJson)) {
         this.skuAttrs = Collections.emptyList();
      } else {
         this.skuAttrs = (List)objectMapper.readValue(skuAttrsJson, new TypeReference() {
         });
      }

   }

   public void setOtherAttrsJson(String otherAttrsJson) throws Exception {
      this.otherAttrsJson = otherAttrsJson;
      if(Strings.isNullOrEmpty(otherAttrsJson)) {
         this.otherAttrs = Collections.emptyList();
      } else {
         this.otherAttrs = (List)objectMapper.readValue(otherAttrsJson, new TypeReference() {
         });
      }

   }

   public String toString() {
      return "ItemSnapshot(itemId=" + this.getItemId() + ", itemInfoMd5=" + this.getItemInfoMd5() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ItemSnapshot)) {
         return false;
      } else {
         ItemSnapshot other = (ItemSnapshot)o;
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

            Object this$itemInfoMd5 = this.getItemInfoMd5();
            Object other$itemInfoMd5 = other.getItemInfoMd5();
            if(this$itemInfoMd5 == null) {
               if(other$itemInfoMd5 != null) {
                  return false;
               }
            } else if(!this$itemInfoMd5.equals(other$itemInfoMd5)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ItemSnapshot;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $itemId = this.getItemId();
      result = result * 59 + ($itemId == null?0:$itemId.hashCode());
      Object $itemInfoMd5 = this.getItemInfoMd5();
      result = result * 59 + ($itemInfoMd5 == null?0:$itemInfoMd5.hashCode());
      return result;
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getItemId() {
      return this.itemId;
   }

   public void setItemId(Long itemId) {
      this.itemId = itemId;
   }

   public String getItemCode() {
      return this.itemCode;
   }

   public void setItemCode(String itemCode) {
      this.itemCode = itemCode;
   }

   public String getItemInfoMd5() {
      return this.itemInfoMd5;
   }

   public void setItemInfoMd5(String itemInfoMd5) {
      this.itemInfoMd5 = itemInfoMd5;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public String getShopName() {
      return this.shopName;
   }

   public void setShopName(String shopName) {
      this.shopName = shopName;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getMainImage() {
      return this.mainImage;
   }

   public void setMainImage(String mainImage) {
      this.mainImage = mainImage;
   }

   public List getImages() {
      return this.images;
   }

   public String getImagesJson() {
      return this.imagesJson;
   }

   public String getAdvertise() {
      return this.advertise;
   }

   public void setAdvertise(String advertise) {
      this.advertise = advertise;
   }

   public String getSpecification() {
      return this.specification;
   }

   public void setSpecification(String specification) {
      this.specification = specification;
   }

   public String getExtraJson() {
      return this.extraJson;
   }

   public Map getExtra() {
      return this.extra;
   }

   public Map getTags() {
      return this.tags;
   }

   public String getTagsJson() {
      return this.tagsJson;
   }

   public List getSkuAttrs() {
      return this.skuAttrs;
   }

   public List getOtherAttrs() {
      return this.otherAttrs;
   }

   public String getOtherAttrsJson() {
      return this.otherAttrsJson;
   }

   public String getSkuAttrsJson() {
      return this.skuAttrsJson;
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
}
