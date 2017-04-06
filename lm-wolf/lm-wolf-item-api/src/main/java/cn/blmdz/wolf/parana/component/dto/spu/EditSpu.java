package cn.blmdz.wolf.parana.component.dto.spu;

import java.util.List;

import cn.blmdz.wolf.parana.rule.dto.BaseOutput;
import cn.blmdz.wolf.parana.spu.model.Spu;
import cn.blmdz.wolf.parana.spu.model.SpuDetail;

public class EditSpu extends BaseOutput {
   private static final long serialVersionUID = 1636430077401940427L;
   private Spu spu;
   private List skuTemplates;
   private SpuDetail spuDetail;

   public void setGeneralSku(List generalSkus) {
      this.setSkuTemplates(generalSkus);
   }

   public Spu getSpu() {
      return this.spu;
   }

   public List getSkuTemplates() {
      return this.skuTemplates;
   }

   public SpuDetail getSpuDetail() {
      return this.spuDetail;
   }

   public void setSpu(Spu spu) {
      this.spu = spu;
   }

   public void setSkuTemplates(List skuTemplates) {
      this.skuTemplates = skuTemplates;
   }

   public void setSpuDetail(SpuDetail spuDetail) {
      this.spuDetail = spuDetail;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof EditSpu)) {
         return false;
      } else {
         EditSpu other = (EditSpu)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$spu = this.getSpu();
            Object other$spu = other.getSpu();
            if(this$spu == null) {
               if(other$spu != null) {
                  return false;
               }
            } else if(!this$spu.equals(other$spu)) {
               return false;
            }

            Object this$skuTemplates = this.getSkuTemplates();
            Object other$skuTemplates = other.getSkuTemplates();
            if(this$skuTemplates == null) {
               if(other$skuTemplates != null) {
                  return false;
               }
            } else if(!this$skuTemplates.equals(other$skuTemplates)) {
               return false;
            }

            Object this$spuDetail = this.getSpuDetail();
            Object other$spuDetail = other.getSpuDetail();
            if(this$spuDetail == null) {
               if(other$spuDetail != null) {
                  return false;
               }
            } else if(!this$spuDetail.equals(other$spuDetail)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof EditSpu;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $spu = this.getSpu();
      result = result * 59 + ($spu == null?0:$spu.hashCode());
      Object $skuTemplates = this.getSkuTemplates();
      result = result * 59 + ($skuTemplates == null?0:$skuTemplates.hashCode());
      Object $spuDetail = this.getSpuDetail();
      result = result * 59 + ($spuDetail == null?0:$spuDetail.hashCode());
      return result;
   }

   public String toString() {
      return "EditSpu(spu=" + this.getSpu() + ", skuTemplates=" + this.getSkuTemplates() + ", spuDetail=" + this.getSpuDetail() + ")";
   }
}
