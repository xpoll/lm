package io.terminus.parana.search.impl;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Sets;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Splitters;
import io.terminus.parana.brand.model.Brand;
import io.terminus.parana.cache.BackCategoryCacher;
import io.terminus.parana.cache.BrandCacher;
import io.terminus.parana.cache.CategoryBindingCacher;
import io.terminus.parana.cache.FrontCategoryCacher;
import io.terminus.parana.category.dto.FrontCategoryTree;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.model.FrontCategory;
import io.terminus.parana.search.dto.AggNav;
import io.terminus.parana.search.dto.Chosen;
import io.terminus.parana.search.dto.GroupedAggNav;
import io.terminus.parana.search.dto.IdAndName;
import io.terminus.parana.search.dto.NameAndCount;
import io.terminus.parana.search.dto.SearchWithAggs;
import io.terminus.parana.search.dto.SearchedItem;
import io.terminus.parana.search.impl.BaseItemQueryBuilder;
import io.terminus.parana.search.item.ItemSearchReadService;
import io.terminus.parana.search.item.SearchItemProperties;
import io.terminus.search.api.Searcher;
import io.terminus.search.api.model.WithAggregations;
import io.terminus.search.api.query.Criterias;
import io.terminus.search.model.Bucket;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class ItemSearchReadServiceImpl implements ItemSearchReadService {
   private static final Logger log = LoggerFactory.getLogger(ItemSearchReadServiceImpl.class);
   private static final String CAT_AGGS = "cat_aggs";
   private static final String ATTR_AGGS = "attr_aggs";
   private static final String BRAND_AGGS = "brand_aggs";
   private final SearchItemProperties searchItemProperties;
   private final BaseItemQueryBuilder itemQueryBuilder;
   private final Searcher searcher;
   private final BackCategoryCacher backCategoryCacher;
   private final CategoryBindingCacher categoryBindingCacher;
   private final FrontCategoryCacher frontCategoryCacher;
   private final BrandCacher brandCacher;

   @Autowired
   public ItemSearchReadServiceImpl(SearchItemProperties searchItemProperties, BaseItemQueryBuilder itemQueryBuilder, Searcher searcher, BackCategoryCacher backCategoryCacher, CategoryBindingCacher categoryBindingCacher, FrontCategoryCacher frontCategoryCacher, BrandCacher brandCacher) {
      this.searchItemProperties = searchItemProperties;
      this.itemQueryBuilder = itemQueryBuilder;
      this.searcher = searcher;
      this.backCategoryCacher = backCategoryCacher;
      this.categoryBindingCacher = categoryBindingCacher;
      this.frontCategoryCacher = frontCategoryCacher;
      this.brandCacher = brandCacher;
   }

   public Response searchWithAggs(Integer pageNo, Integer pageSize, String templateName, Map params) {
      String brandIds = (String)params.get("bids");
      String aggSpecifiers = this.makeAggSpecifiers(brandIds);
      params.put("aggs", aggSpecifiers);
      String frontCategoryId = (String)params.get("fcid");
      FrontCategory frontCategory = null;
      if(StringUtils.hasText(frontCategoryId)) {
         List<FrontCategoryTree> frontCategoryTrees = this.frontCategoryCacher.findByIds(frontCategoryId);
         if(CollectionUtils.isEmpty(frontCategoryTrees)) {
            SearchWithAggs searchWithAggs = new SearchWithAggs();
            searchWithAggs.setChosen(Collections.emptyList());
            searchWithAggs.setBackCategories(Collections.emptyList());
            searchWithAggs.setEntities(Paging.empty(SearchedItem.class));
            searchWithAggs.setBrands(Collections.emptyList());
            searchWithAggs.setAttributes(Collections.emptyList());
            searchWithAggs.setBreadCrumbs(Lists.newArrayList(new IdAndName[]{new IdAndName(Long.valueOf(0L), "所有类目")}));
            return Response.ok(searchWithAggs);
         }

         List<Long> leafFcids = Lists.newArrayList();
         FrontCategoryTree frontCategoryTree = (FrontCategoryTree)frontCategoryTrees.get(0);
         frontCategory = frontCategoryTree.getCurrent();
         this.leafOf(frontCategoryTree, leafFcids);
         List<Long> backCategoryIds = Lists.newArrayList();

         for(Long leafFcid : leafFcids) {
            backCategoryIds.addAll(this.categoryBindingCacher.findByFrontCategoryId(leafFcid));
         }

         if(CollectionUtils.isEmpty(backCategoryIds)) {
            SearchWithAggs searchWithAggs = new SearchWithAggs();
            searchWithAggs.setChosen(Lists.newArrayList(new Chosen[]{new Chosen(3, frontCategoryId, frontCategory.getName())}));
            searchWithAggs.setBackCategories(Collections.emptyList());
            searchWithAggs.setEntities(Paging.empty(SearchedItem.class));
            searchWithAggs.setBrands(Collections.emptyList());
            searchWithAggs.setAttributes(Collections.emptyList());
            searchWithAggs.setBreadCrumbs(Lists.newArrayList(new IdAndName[]{new IdAndName(Long.valueOf(0L), "所有类目")}));
            return Response.ok(searchWithAggs);
         }

         String bcidsFromFcids = Joiner.on('_').skipNulls().join(backCategoryIds);
         String bcids = (String)params.get("bcids");
         bcids = Joiner.on('_').skipNulls().join(bcids, bcidsFromFcids, new Object[0]);
         params.put("bcids", bcids);
      }

      String q = (String)params.get("q");
      if(StringUtils.hasText(q)) {
         params.put("highlight", "name");
      }

      Criterias criterias = this.itemQueryBuilder.makeCriterias(pageNo, pageSize, params);
      WithAggregations<SearchedItem> withAggs = this.searcher.searchWithAggs(this.searchItemProperties.getIndexName(), this.searchItemProperties.getIndexType(), templateName, criterias, SearchedItem.class);
      Long total = withAggs.getTotal();
      List<SearchedItem> data = withAggs.getData();
      SearchWithAggs searchWithAggs = new SearchWithAggs();
      Paging<SearchedItem> entities = new Paging(total, data);
      searchWithAggs.setEntities(entities);
      Map<String, List<Bucket>> aggregations = withAggs.getAggregations();
      List<Bucket> catAggs = (List)aggregations.get("cat_aggs");
      String chosenCats = (String)params.get("bcids");
      Set<String> chosenCategories = Sets.newHashSet();
      if(StringUtils.hasText(chosenCats)) {
         for(String chosenCat : Splitters.UNDERSCORE.split(chosenCats)) {
            chosenCategories.add(chosenCat);
         }
      }

      List<AggNav> catNavs = this.makeCategoryNavs(catAggs, chosenCategories);
      searchWithAggs.setBackCategories(catNavs);
      List<Bucket> brandAggs = (List)aggregations.get("brand_aggs");
      List<AggNav> brandNavs = this.makeBrandNavs(brandAggs);
      searchWithAggs.setBrands(brandNavs);
      String chosenAttrs = (String)params.get("attrs");
      List<Bucket> attrAggs = (List)aggregations.get("attr_aggs");
      List<GroupedAggNav> attrNavs = this.makeGroupedAttrNavs(attrAggs, chosenAttrs);
      searchWithAggs.setAttributes(attrNavs);
      Long chosenCategoryId = Long.valueOf(0L);
      if(chosenCategories.size() == 1) {
         chosenCategoryId = Long.valueOf((String)Iterables.get(chosenCategories, 0));
      } else if(catNavs != null && catNavs.size() == 1) {
         chosenCategoryId = (Long)((AggNav)catNavs.get(0)).getKey();
      }

      List<IdAndName> breadCrumbs = this.makeBreadCrumbs(chosenCategoryId);
      searchWithAggs.setBreadCrumbs(breadCrumbs);
      List<Chosen> chosens = Lists.newArrayList();
      if(StringUtils.hasText(brandIds)) {
         for(String brandId : Splitters.UNDERSCORE.split(brandIds)) {
            Brand brand = this.brandCacher.findBrandById(Long.valueOf(brandId));
            chosens.add(new Chosen(1, brandId, brand.getName()));
         }
      }

      if(StringUtils.hasText(chosenAttrs)) {
         for(String attr : Splitters.UNDERSCORE.splitToList(chosenAttrs)) {
            List<String> parts = Splitters.COLON.splitToList(attr);
            chosens.add(new Chosen(2, parts.get(0), (String)parts.get(1)));
         }
      }

      if(frontCategory != null) {
         chosens.add(new Chosen(3, frontCategory.getId(), frontCategory.getName()));
      }

      searchWithAggs.setChosen(chosens);
      return Response.ok(searchWithAggs);
   }

   private String makeAggSpecifiers(String brandId) {
      StringBuilder sb = new StringBuilder("attr_aggs:attributes:100");
      if(!StringUtils.hasText(brandId)) {
         sb.append("$brand_aggs:brandId:20");
      }

      sb.append("$cat_aggs:categoryIds:20");
      return sb.toString();
   }

   private List makeCategoryNavs(List catAggs, Set chosenCategories) {
      if(CollectionUtils.isEmpty(catAggs)) {
         return null;
      } else {
         List<AggNav> catNavs = Lists.newArrayListWithCapacity(catAggs.size());

         for(Bucket catAgg : catAggs) {
            String key = catAgg.getKey();
            if(!chosenCategories.contains(key)) {
               try {
                  Long categoryId = Long.valueOf(key);
                  BackCategory backCategory = this.backCategoryCacher.findBackCategoryById(categoryId);
                  if(!backCategory.getHasChildren().booleanValue()) {
                     AggNav aggNav = new AggNav(categoryId, backCategory.getName(), catAgg.getDoc_count());
                     catNavs.add(aggNav);
                  }
               } catch (Exception var10) {
                  log.error("failed to build cat navs for bucket(key={}),cause:{}", key, Throwables.getStackTraceAsString(var10));
               }
            }
         }

         return catNavs;
      }
   }

   private List makeGroupedAttrNavs(List attrAggs, String chosenAttrs) {
      if(CollectionUtils.isEmpty(attrAggs)) {
         return null;
      } else {
         List<AggNav> attrNavs = Lists.newArrayListWithCapacity(attrAggs.size());
         Set<String> chosenKeys = Sets.newHashSet();
         if(StringUtils.hasText(chosenAttrs)) {
            for(String attr : Splitters.UNDERSCORE.splitToList(chosenAttrs)) {
               List<String> parts = Splitters.COLON.splitToList(attr);
               chosenKeys.add(parts.get(0));
            }
         }

         for(Bucket attrAgg : attrAggs) {
            List<String> keyAndVal = Splitters.COLON.splitToList(attrAgg.getKey());
            if(!chosenKeys.contains(keyAndVal.get(0))) {
               AggNav aggNav = new AggNav(keyAndVal.get(0), (String)keyAndVal.get(1), attrAgg.getDoc_count());
               attrNavs.add(aggNav);
            }
         }

         Multimap<Object, AggNav> byKey = Multimaps.index(attrNavs, new Function() {
            public Object apply(AggNav attrNav) {
               return attrNav.getKey();
            }
         });
         List<GroupedAggNav> result = Lists.newArrayListWithCapacity(5);

         for(Object key : Iterables.limit(byKey.keySet(), 5)) {
            Iterable<AggNav> aggNavs = byKey.get(key);
            String group = String.valueOf(key);
            List<NameAndCount> nameAndCounts = Lists.newArrayListWithCapacity(10);

            for(AggNav aggNav : Iterables.limit(aggNavs, 10)) {
               nameAndCounts.add(new NameAndCount(aggNav.getName(), Integer.valueOf(aggNav.getCount().intValue())));
            }

            result.add(new GroupedAggNav(group, nameAndCounts));
         }

         return result;
      }
   }

   private List makeBrandNavs(List brandAggs) {
      if(CollectionUtils.isEmpty(brandAggs)) {
         return null;
      } else {
         List<AggNav> brandNavs = Lists.newArrayListWithCapacity(brandAggs.size());

         for(Bucket brandAgg : brandAggs) {
            try {
               Long brandId = Long.valueOf(brandAgg.getKey());
               Brand brand = this.brandCacher.findBrandById(brandId);
               AggNav aggNav = new AggNav(brandId, brand.getName(), brandAgg.getDoc_count());
               brandNavs.add(aggNav);
            } catch (Exception var8) {
               log.error("failed to build brand navs for bucket(key={}),cause:{}", brandAgg.getKey(), Throwables.getStackTraceAsString(var8));
            }
         }

         return brandNavs;
      }
   }

   private List makeBreadCrumbs(Long currentCategoryId) {
      List<IdAndName> idAndNames;
      BackCategory backCategory;
      for(idAndNames = Lists.newArrayList(); currentCategoryId.longValue() > 0L; currentCategoryId = backCategory.getPid()) {
         backCategory = this.backCategoryCacher.findBackCategoryById(currentCategoryId);
         idAndNames.add(new IdAndName(backCategory.getId(), backCategory.getName()));
      }

      backCategory = Lists.newArrayListWithCapacity(idAndNames.size() + 1);
      backCategory.add(new IdAndName(Long.valueOf(0L), "所有分类"));
      backCategory.addAll(Lists.reverse(idAndNames));
      return backCategory;
   }

   private void leafOf(FrontCategoryTree frontCategoryTree, List leafFcids) {
      if(CollectionUtils.isEmpty(frontCategoryTree.getChildren())) {
         leafFcids.add(frontCategoryTree.getCurrent().getId());
      } else {
         for(FrontCategoryTree categoryTree : frontCategoryTree.getChildren()) {
            this.leafOf(categoryTree, leafFcids);
         }
      }

   }
}
