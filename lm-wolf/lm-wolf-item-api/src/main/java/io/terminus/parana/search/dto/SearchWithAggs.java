package io.terminus.parana.search.dto;

import io.terminus.common.model.Paging;
import java.io.Serializable;
import java.util.List;

public class SearchWithAggs implements Serializable {
   private static final long serialVersionUID = 8702350543499146460L;
   private Paging entities;
   private List attributes;
   private List brands;
   private List backCategories;
   private List breadCrumbs;
   private List chosen;

   public Paging getEntities() {
      return this.entities;
   }

   public List getAttributes() {
      return this.attributes;
   }

   public List getBrands() {
      return this.brands;
   }

   public List getBackCategories() {
      return this.backCategories;
   }

   public List getBreadCrumbs() {
      return this.breadCrumbs;
   }

   public List getChosen() {
      return this.chosen;
   }

   public void setEntities(Paging entities) {
      this.entities = entities;
   }

   public void setAttributes(List attributes) {
      this.attributes = attributes;
   }

   public void setBrands(List brands) {
      this.brands = brands;
   }

   public void setBackCategories(List backCategories) {
      this.backCategories = backCategories;
   }

   public void setBreadCrumbs(List breadCrumbs) {
      this.breadCrumbs = breadCrumbs;
   }

   public void setChosen(List chosen) {
      this.chosen = chosen;
   }
}
