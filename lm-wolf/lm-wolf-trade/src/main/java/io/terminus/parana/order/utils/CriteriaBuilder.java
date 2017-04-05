package io.terminus.parana.order.utils;

import com.google.common.base.Strings;
import io.terminus.common.model.PageInfo;
import io.terminus.common.utils.Splitters;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.util.CollectionUtils;

public class CriteriaBuilder {
   private static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd");
   private Map criteria;

   private CriteriaBuilder(Map criteria) {
      this.criteria = criteria;
   }

   public Map toMap() {
      return this.criteria;
   }

   public static class Builder {
      private Map params = new HashMap();

      public CriteriaBuilder.Builder buyerId(Long buyerId) {
         if(null != buyerId) {
            this.params.put("buyerId", buyerId);
         }

         return this;
      }

      public CriteriaBuilder.Builder shopId(Long shopId) {
         if(null != shopId) {
            this.params.put("shopId", shopId);
         }

         return this;
      }

      public CriteriaBuilder.Builder orderId(Long orderId) {
         if(null != orderId) {
            this.params.put("id", orderId);
         }

         return this;
      }

      public CriteriaBuilder.Builder orderIds(List orderIds) {
         if(!CollectionUtils.isEmpty(orderIds)) {
            this.params.put("ids", orderIds);
         }

         return this;
      }

      public CriteriaBuilder.Builder parentIds(List parentIds) {
         if(!CollectionUtils.isEmpty(parentIds)) {
            this.params.put("parentIds", parentIds);
         }

         return this;
      }

      public CriteriaBuilder.Builder startAt(String startAt) {
         if(!Strings.isNullOrEmpty(startAt)) {
            Date start = CriteriaBuilder.DATE_TIME_FORMAT.parseDateTime(startAt).withTimeAtStartOfDay().toDate();
            this.params.put("startAt", start);
         }

         return this;
      }

      public CriteriaBuilder.Builder endAt(String endAt) {
         if(!Strings.isNullOrEmpty(endAt)) {
            Date end = CriteriaBuilder.DATE_TIME_FORMAT.parseDateTime(endAt).plusDays(1).withTimeAtStartOfDay().toDate();
            this.params.put("endAt", end);
         }

         return this;
      }

      public CriteriaBuilder.Builder nids(String nids) {
         if(!Strings.isNullOrEmpty(nids)) {
            this.params.put("nids", Splitters.COMMA.omitEmptyStrings().trimResults().splitToList(nids));
         }

         return this;
      }

      public CriteriaBuilder.Builder pageInfo(Integer pageNo, Integer size) {
         PageInfo pageInfo = PageInfo.of(pageNo, size);
         this.params.put("offset", pageInfo.getOffset());
         this.params.put("limit", pageInfo.getLimit());
         return this;
      }

      public CriteriaBuilder build() {
         return new CriteriaBuilder(this.params);
      }
   }
}
