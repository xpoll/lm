package cn.blmdz.wolf.parana.search.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

public class IndexedItem implements Serializable {
   private static final long serialVersionUID = -6845362800121011375L;
   private Long id;
   private String name;
   private String itemCode;
   private Long spuId;
   private Long shopId;
   private String shopName;
   private Long brandId;
   private String brandName;
   private String mainImage;
   private Integer price;
   private Integer stockQuantity;
   private Integer saleQuantity;
   private Integer status;
   private String specification;
   private Integer type;
   private List categoryIds;
   private List attributes;
   private List shopCategoryIds;
   private List regionIds;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getItemCode() {
      return this.itemCode;
   }

   public void setItemCode(String itemCode) {
      this.itemCode = itemCode;
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

   public String getMainImage() {
      return this.mainImage;
   }

   public void setMainImage(String mainImage) {
      this.mainImage = mainImage;
   }

   public Integer getPrice() {
      return this.price;
   }

   public void setPrice(Integer price) {
      this.price = price;
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

   public List getCategoryIds() {
      return this.categoryIds;
   }

   public void setCategoryIds(List categoryIds) {
      this.categoryIds = categoryIds;
   }

   public List getAttributes() {
      return this.attributes;
   }

   public void setAttributes(List attributes) {
      this.attributes = attributes;
   }

   public List getShopCategoryIds() {
      return this.shopCategoryIds;
   }

   public void setShopCategoryIds(List shopCategoryIds) {
      this.shopCategoryIds = shopCategoryIds;
   }

   public List getRegionIds() {
      return this.regionIds;
   }

   public void setRegionIds(List regionIds) {
      this.regionIds = regionIds;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
