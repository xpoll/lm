package cn.blmdz.wolf.parana.item.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Strings;

import cn.blmdz.home.common.util.JsonMapper;
import cn.blmdz.wolf.common.constants.JacksonType;

public class Item implements Serializable {
   private static final long serialVersionUID = -4535812928439539071L;
   private static final ObjectMapper objectMapper = JsonMapper.nonEmptyMapper().getMapper();
   private Long id;
   private String itemCode;
   private Long categoryId;
   private Long spuId;
   private Long shopId;
   private String shopName;
   private Long brandId;
   private String brandName;
   private String name;
   private String mainImage;
   private Integer lowPrice;
   private Integer highPrice;
   private Integer stockType;
   private Integer stockQuantity;
   private Integer saleQuantity;
   private Integer status;
   private Date onShelfAt;
   private String advertise;
   private String specification;
   private Integer type;
   private Integer reduceStockType;
   @JsonIgnore
   private String extraJson;
   private Map extra;
   private Map tags;
   private String tagsJson;
   private String itemInfoMd5;
   @JsonIgnore
   private Date createdAt;
   @JsonIgnore
   private Date updatedAt;

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

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Item)) {
         return false;
      } else {
         Item other = (Item)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$itemCode = this.getItemCode();
            Object other$itemCode = other.getItemCode();
            if(this$itemCode == null) {
               if(other$itemCode != null) {
                  return false;
               }
            } else if(!this$itemCode.equals(other$itemCode)) {
               return false;
            }

            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$specification = this.getSpecification();
            Object other$specification = other.getSpecification();
            if(this$specification == null) {
               if(other$specification != null) {
                  return false;
               }
            } else if(!this$specification.equals(other$specification)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Item;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $itemCode = this.getItemCode();
      result = result * 59 + ($itemCode == null?0:$itemCode.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $specification = this.getSpecification();
      result = result * 59 + ($specification == null?0:$specification.hashCode());
      return result;
   }

   public String toString() {
      return "Item(id=" + this.getId() + ", itemCode=" + this.getItemCode() + ", categoryId=" + this.getCategoryId() + ", spuId=" + this.getSpuId() + ", shopId=" + this.getShopId() + ", shopName=" + this.getShopName() + ", brandId=" + this.getBrandId() + ", brandName=" + this.getBrandName() + ", name=" + this.getName() + ", mainImage=" + this.getMainImage() + ", lowPrice=" + this.getLowPrice() + ", highPrice=" + this.getHighPrice() + ", stockType=" + this.getStockType() + ", stockQuantity=" + this.getStockQuantity() + ", saleQuantity=" + this.getSaleQuantity() + ", status=" + this.getStatus() + ", onShelfAt=" + this.getOnShelfAt() + ", advertise=" + this.getAdvertise() + ", specification=" + this.getSpecification() + ", type=" + this.getType() + ", reduceStockType=" + this.getReduceStockType() + ", extraJson=" + this.getExtraJson() + ", extra=" + this.getExtra() + ", tags=" + this.getTags() + ", tagsJson=" + this.getTagsJson() + ", itemInfoMd5=" + this.getItemInfoMd5() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getItemCode() {
      return this.itemCode;
   }

   public void setItemCode(String itemCode) {
      this.itemCode = itemCode;
   }

   public Long getCategoryId() {
      return this.categoryId;
   }

   public void setCategoryId(Long categoryId) {
      this.categoryId = categoryId;
   }

   public Long getSpuId() {
      return this.spuId;
   }

   public void setSpuId(Long spuId) {
      this.spuId = spuId;
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

   public Long getBrandId() {
      return this.brandId;
   }

   public void setBrandId(Long brandId) {
      this.brandId = brandId;
   }

   public String getBrandName() {
      return this.brandName;
   }

   public void setBrandName(String brandName) {
      this.brandName = brandName;
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

   public Integer getLowPrice() {
      return this.lowPrice;
   }

   public void setLowPrice(Integer lowPrice) {
      this.lowPrice = lowPrice;
   }

   public Integer getHighPrice() {
      return this.highPrice;
   }

   public void setHighPrice(Integer highPrice) {
      this.highPrice = highPrice;
   }

   public Integer getStockType() {
      return this.stockType;
   }

   public void setStockType(Integer stockType) {
      this.stockType = stockType;
   }

   public Integer getStockQuantity() {
      return this.stockQuantity;
   }

   public void setStockQuantity(Integer stockQuantity) {
      this.stockQuantity = stockQuantity;
   }

   public Integer getSaleQuantity() {
      return this.saleQuantity;
   }

   public void setSaleQuantity(Integer saleQuantity) {
      this.saleQuantity = saleQuantity;
   }

   public Integer getStatus() {
      return this.status;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public Date getOnShelfAt() {
      return this.onShelfAt;
   }

   public void setOnShelfAt(Date onShelfAt) {
      this.onShelfAt = onShelfAt;
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

   public Integer getType() {
      return this.type;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public Integer getReduceStockType() {
      return this.reduceStockType;
   }

   public void setReduceStockType(Integer reduceStockType) {
      this.reduceStockType = reduceStockType;
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

   public String getItemInfoMd5() {
      return this.itemInfoMd5;
   }

   public void setItemInfoMd5(String itemInfoMd5) {
      this.itemInfoMd5 = itemInfoMd5;
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
