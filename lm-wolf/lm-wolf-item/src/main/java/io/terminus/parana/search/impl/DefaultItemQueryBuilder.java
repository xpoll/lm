package io.terminus.parana.search.impl;

import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import io.terminus.common.utils.Splitters;
import io.terminus.parana.search.impl.BaseItemQueryBuilder;
import io.terminus.search.api.query.Aggs;
import io.terminus.search.api.query.Highlight;
import io.terminus.search.api.query.Keyword;
import io.terminus.search.api.query.Range;
import io.terminus.search.api.query.Sort;
import io.terminus.search.api.query.Term;
import io.terminus.search.api.query.Terms;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class DefaultItemQueryBuilder extends BaseItemQueryBuilder {
   private static final Logger log = LoggerFactory.getLogger(DefaultItemQueryBuilder.class);
   private static final Splitter DOLLAR_SPLITTER = Splitter.on('$').omitEmptyStrings().trimResults();

   public Keyword buildKeyword(Map params) {
      String q = (String)params.get("q");
      return StringUtils.hasText(q)?new Keyword(ImmutableList.of("name", "brandName"), q):null;
   }

   public List buildTerm(Map params) {
      List<Term> termList = Lists.newArrayList();
      String bid = (String)params.get("bid");
      if(StringUtils.hasText(bid)) {
         termList.add(new Term("brandId", bid));
      }

      String shopId = (String)params.get("shopId");
      if(StringUtils.hasText(shopId)) {
         termList.add(new Term("shopId", shopId));
      }

      String shopCatId = (String)params.get("shopCatId");
      if(StringUtils.hasText(shopCatId)) {
         termList.add(new Term("shopCategoryId", shopCatId));
      }

      return termList;
   }

   public List buildTerms(Map params) {
      List<Terms> termsList = Lists.newArrayList();
      String attributes = (String)params.get("attrs");
      if(StringUtils.hasText(attributes)) {
         List<String> parts = Splitters.UNDERSCORE.splitToList(attributes);
         ArrayListMultimap<String, String> groupByKey = ArrayListMultimap.create();

         for(String part : parts) {
            List<String> kv = Splitters.COLON.splitToList(part);
            if(kv.size() != 2) {
               log.warn("kv not split by `:\', kv={}", kv);
            } else {
               groupByKey.put(kv.get(0), part);
            }
         }

         for(String key : groupByKey.keySet()) {
            List<String> kvs = groupByKey.get(key);
            if(!kvs.isEmpty()) {
               termsList.add(new Terms("attributes", new ArrayList(kvs)));
            }
         }
      }

      String bids = (String)params.get("bids");
      if(StringUtils.hasText(bids)) {
         List<String> parts = Splitters.UNDERSCORE.splitToList(bids);
         termsList.add(new Terms("brandId", parts));
      }

      String regionIds = (String)params.get("regionIds");
      if(StringUtils.hasText(regionIds)) {
         termsList.add(new Terms("regionIds", Splitters.UNDERSCORE.splitToList(regionIds)));
      }

      String ids = (String)params.get("ids");
      if(StringUtils.hasText(ids)) {
         termsList.add(new Terms("id", Splitters.UNDERSCORE.splitToList(ids)));
      }

      String bcids = (String)params.get("bcids");
      if(StringUtils.hasText(bcids)) {
         termsList.add(new Terms("categoryIds", Splitters.UNDERSCORE.splitToList(bcids)));
      }

      return termsList;
   }

   public List buildRanges(Map params) {
      List<Range> ranges = Lists.newArrayList();
      String p_f = (String)params.get("p_f");
      String p_t = (String)params.get("p_t");
      if(StringUtils.hasText(p_f) || StringUtils.hasText(p_t)) {
         Range lowPrice = new Range("price", p_f, p_t);
         ranges.add(lowPrice);
      }

      return ranges;
   }

   public List buildAggs(Map params) {
      String aggs = (String)params.get("aggs");
      if(!StringUtils.hasText(aggs)) {
         return null;
      } else {
         List<String> aggSpecifiers = DOLLAR_SPLITTER.splitToList(aggs);
         List<Aggs> result = Lists.newArrayListWithCapacity(aggSpecifiers.size());

         for(String aggSpecifier : aggSpecifiers) {
            List<String> parts = Splitters.COLON.splitToList(aggSpecifier);
            Aggs agg = new Aggs((String)parts.get(0), (String)parts.get(1), Integer.valueOf(Integer.parseInt((String)parts.get(2))));
            result.add(agg);
         }

         return result;
      }
   }

   public List buildSort(Map params) {
      List<Sort> sorts = Lists.newArrayList();
      String sort = (String)params.get("sort");
      if(!Strings.isNullOrEmpty(sort)) {
         List<String> parts = Splitters.UNDERSCORE.splitToList(sort);
         if(parts.size() < 4) {
            return Collections.emptyList();
         }

         String price = (String)Iterables.getFirst(parts, "0");
         String stockQuantity = (String)Iterables.get(parts, 1, "0");
         String saleQuantity = (String)Iterables.get(parts, 2, "0");
         String createdAt = (String)Iterables.get(parts, 3, "0");
         switch(Integer.parseInt(price)) {
         case 1:
            sorts.add(new Sort("price", "asc"));
            break;
         case 2:
            sorts.add(new Sort("price", "desc"));
         }

         switch(Integer.parseInt(stockQuantity)) {
         case 1:
            sorts.add(new Sort("stockQuantity", "asc"));
            break;
         case 2:
            sorts.add(new Sort("stockQuantity", "desc"));
         }

         switch(Integer.parseInt(saleQuantity)) {
         case 1:
            sorts.add(new Sort("saleQuantity", "asc"));
            break;
         case 2:
            sorts.add(new Sort("saleQuantity", "desc"));
         }

         switch(Integer.parseInt(createdAt)) {
         case 1:
            sorts.add(new Sort("createdAt", "asc"));
            break;
         case 2:
            sorts.add(new Sort("createdAt", "desc"));
         }
      }

      return sorts;
   }

   public List buildHighlight(Map params) {
      String highlight = (String)params.get("highlight");
      if(!StringUtils.hasText(highlight)) {
         return null;
      } else {
         List<String> fields = Splitters.UNDERSCORE.splitToList(highlight);
         List<Highlight> highlights = Lists.newArrayListWithCapacity(fields.size());

         for(String field : fields) {
            highlights.add(new Highlight(field));
         }

         return highlights;
      }
   }
}
