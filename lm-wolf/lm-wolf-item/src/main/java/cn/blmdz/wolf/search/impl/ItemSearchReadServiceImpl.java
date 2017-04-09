package cn.blmdz.wolf.search.impl;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.Sets;

import cn.blmdz.home.common.model.Paging;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.home.search.api.Searcher;
import cn.blmdz.home.search.api.model.WithAggregations;
import cn.blmdz.home.search.api.query.Criterias;
import cn.blmdz.home.search.model.Bucket;
import cn.blmdz.wolf.brand.model.Brand;
import cn.blmdz.wolf.cache.BackCategoryCacher;
import cn.blmdz.wolf.cache.BrandCacher;
import cn.blmdz.wolf.cache.CategoryBindingCacher;
import cn.blmdz.wolf.cache.FrontCategoryCacher;
import cn.blmdz.wolf.cache.ShopCategoryCacher;
import cn.blmdz.wolf.category.dto.FrontCategoryTree;
import cn.blmdz.wolf.category.model.BackCategory;
import cn.blmdz.wolf.category.model.FrontCategory;
import cn.blmdz.wolf.category.model.ShopCategory;
import cn.blmdz.wolf.search.dto.AggNav;
import cn.blmdz.wolf.search.dto.Chosen;
import cn.blmdz.wolf.search.dto.GroupedAggNav;
import cn.blmdz.wolf.search.dto.IdAndName;
import cn.blmdz.wolf.search.dto.NameAndCount;
import cn.blmdz.wolf.search.dto.SearchedItem;
import cn.blmdz.wolf.search.dto.SearchedItemInShopWithAggs;
import cn.blmdz.wolf.search.dto.SearchedItemWithAggs;
import cn.blmdz.wolf.search.item.ItemSearchReadService;
import cn.blmdz.wolf.search.item.SearchItemProperties;

@Service
public class ItemSearchReadServiceImpl implements ItemSearchReadService {
	private static final Logger log = LoggerFactory.getLogger(ItemSearchReadServiceImpl.class);
	private static final String CAT_AGGS = "cat_aggs";
	private static final String ATTR_AGGS = "attr_aggs";
	private static final String BRAND_AGGS = "brand_aggs";
	private static final String SHOP_CATEGORY_AGGS = "shop_category_aggs";
	private final SearchItemProperties searchItemProperties;
	private final BaseItemQueryBuilder itemQueryBuilder;
	private final Searcher searcher;
	private final BackCategoryCacher backCategoryCacher;
	private final CategoryBindingCacher categoryBindingCacher;
	private final FrontCategoryCacher frontCategoryCacher;
	private final BrandCacher brandCacher;
	private final ShopCategoryCacher shopCategoryCacher;

	@Autowired
	public ItemSearchReadServiceImpl(SearchItemProperties searchItemProperties, BaseItemQueryBuilder itemQueryBuilder,
			Searcher searcher, BackCategoryCacher backCategoryCacher, CategoryBindingCacher categoryBindingCacher,
			FrontCategoryCacher frontCategoryCacher, BrandCacher brandCacher, ShopCategoryCacher shopCategoryCacher) {
		this.searchItemProperties = searchItemProperties;
		this.itemQueryBuilder = itemQueryBuilder;
		this.searcher = searcher;
		this.backCategoryCacher = backCategoryCacher;
		this.categoryBindingCacher = categoryBindingCacher;
		this.frontCategoryCacher = frontCategoryCacher;
		this.brandCacher = brandCacher;
		this.shopCategoryCacher = shopCategoryCacher;
	}

	public Response<SearchedItemWithAggs> searchWithAggs(Integer pageNo, Integer pageSize, String templateName,
			Map<String, String> params) {
		String brandIds = (String) params.get("bids");
		String aggSpecifiers = makeAggSpecifiers(brandIds);
		params.put("aggs", aggSpecifiers);

		String frontCategoryId = (String) params.get("fcid");
		FrontCategory frontCategory = null;
		if (StringUtils.hasText(frontCategoryId)) {
			List frontCategoryTrees = this.frontCategoryCacher.findByIds(frontCategoryId);
			if (!CollectionUtils.isEmpty(frontCategoryTrees)) {
				List<Long> leafFcids = Lists.newArrayList();
				FrontCategoryTree frontCategoryTree = (FrontCategoryTree) frontCategoryTrees.get(0);
				frontCategory = frontCategoryTree.getCurrent();
				leafOf(frontCategoryTree, leafFcids);

				List backCategoryIds = Lists.newArrayList();
				for (Long leafFcid : leafFcids) {
					backCategoryIds.addAll(this.categoryBindingCacher.findByFrontCategoryId(leafFcid));
				}

				if (!CollectionUtils.isEmpty(backCategoryIds)) {
					String bcidsFromFcids = Joiner.on('_').skipNulls().join(backCategoryIds);
					String bcids = (String) params.get("bcids");
					bcids = Joiner.on('_').skipNulls().join(bcids, bcidsFromFcids, new Object[0]);
					params.put("bcids", bcids);
				} else {
					SearchedItemWithAggs searchWithAggs = new SearchedItemWithAggs();
					searchWithAggs.setChosen(Lists
							.newArrayList(new Chosen[] { new Chosen(3, frontCategoryId, frontCategory.getName()) }));
					searchWithAggs.setBackCategories(Collections.<AggNav>emptyList());
					searchWithAggs.setEntities(Paging.empty(SearchedItem.class));
					searchWithAggs.setBrands(Collections.<AggNav>emptyList());
					searchWithAggs.setAttributes(Collections.<GroupedAggNav>emptyList());
					searchWithAggs.setBreadCrumbs(
							Lists.newArrayList(new IdAndName[] { new IdAndName(Long.valueOf(0L), "所有类目") }));
					return Response.ok(searchWithAggs);
				}
			} else {
				SearchedItemWithAggs searchWithAggs = new SearchedItemWithAggs();
				searchWithAggs.setChosen(Collections.<Chosen>emptyList());
				searchWithAggs.setBackCategories(Collections.<AggNav>emptyList());
				searchWithAggs.setEntities(Paging.empty(SearchedItem.class));
				searchWithAggs.setBrands(Collections.<AggNav>emptyList());
				searchWithAggs.setAttributes(Collections.<GroupedAggNav>emptyList());
				searchWithAggs.setBreadCrumbs(
						Lists.newArrayList(new IdAndName[] { new IdAndName(Long.valueOf(0L), "所有类目") }));
				return Response.ok(searchWithAggs);
			}
		}

		setHighlight(params);

		WithAggregations withAggs = doSearch(pageNo, pageSize, templateName, params);

		Long total = withAggs.getTotal();
		List data = withAggs.getData();

		SearchedItemWithAggs searchWithAggs = new SearchedItemWithAggs();

		Paging entities = new Paging(total, data);
		searchWithAggs.setEntities(entities);

		Map aggregations = withAggs.getAggregations();

		List catAggs = (List) aggregations.get("cat_aggs");

		String chosenCats = (String) params.get("bcids");

		Set chosenCategories = Sets.newHashSet();
		if (StringUtils.hasText(chosenCats)) {
			for (String chosenCat : Splitters.UNDERSCORE.split(chosenCats)) {
				chosenCategories.add(chosenCat);
			}
		}

		List catNavs = makeCategoryNavs(catAggs, chosenCategories);
		searchWithAggs.setBackCategories(catNavs);

		List brandAggs = (List) aggregations.get("brand_aggs");
		List brandNavs = makeBrandNavs(brandAggs);
		searchWithAggs.setBrands(brandNavs);

		String chosenAttrs = (String) params.get("attrs");
		List attrAggs = (List) aggregations.get("attr_aggs");
		List attrNavs = makeGroupedAttrNavs(attrAggs, chosenAttrs);
		searchWithAggs.setAttributes(attrNavs);

		Long chosenCategoryId = Long.valueOf(0L);
		if (chosenCategories.size() == 1)
			chosenCategoryId = Long.valueOf((String) Iterables.get(chosenCategories, 0));
		else if ((catNavs != null) && (catNavs.size() == 1)) {
			chosenCategoryId = (Long) ((AggNav) catNavs.get(0)).getKey();
		}
		List breadCrumbs = makeBreadCrumbs(chosenCategoryId);
		searchWithAggs.setBreadCrumbs(breadCrumbs);

		List chosens = Lists.newArrayList();

		addChosenBrands(brandIds, chosens);

		addChosenAttributes(chosenAttrs, chosens);

		addChosenFrontCategory(frontCategory, chosens);

		searchWithAggs.setChosen(chosens);
		return Response.ok(searchWithAggs);
	}

	public Response<SearchedItemInShopWithAggs> searchInShopWithAggs(Integer pageNo, Integer pageSize,
			String templateName, Map<String, String> params) {
		String aggSpecifiers = makeAggSpecifiersOfShop();
		params.put("aggs", aggSpecifiers);

		setHighlight(params);

		WithAggregations withAggs = doSearch(pageNo, pageSize, templateName, params);

		SearchedItemInShopWithAggs searchWithAggs = new SearchedItemInShopWithAggs();

		Paging entities = new Paging(withAggs.getTotal(), withAggs.getData());
		searchWithAggs.setEntities(entities);

		Map aggregations = withAggs.getAggregations();

		String chosenAttrs = (String) params.get("attrs");
		List attrAggs = (List) aggregations.get("attr_aggs");
		List attrNavs = makeGroupedAttrNavs(attrAggs, chosenAttrs);
		searchWithAggs.setAttributes(attrNavs);

		List chosens = Lists.newArrayList();
		addChosenAttributes(chosenAttrs, chosens);

		searchWithAggs.setChosen(chosens);

		return Response.ok(searchWithAggs);
	}

	private String makeAggSpecifiersOfShop() {
		StringBuilder sb = new StringBuilder("attr_aggs:attributes:100");
		sb.append("$shop_category_aggs:shopCategoryIds:20");
		return sb.toString();
	}

	private String makeAggSpecifiers(String brandId) {
		StringBuilder sb = new StringBuilder("attr_aggs:attributes:100");

		if (!StringUtils.hasText(brandId)) {
			sb.append("$brand_aggs:brandId:20");
		}
		sb.append("$cat_aggs:categoryIds:20");
		return sb.toString();
	}

	private void setHighlight(Map<String, String> params) {
		String q = (String) params.get("q");
		if (StringUtils.hasText(q))
			params.put("highlight", "name");
	}

	private WithAggregations<SearchedItem> doSearch(Integer pageNo, Integer pageSize, String templateName,
			Map<String, String> params) {
		Criterias criterias = this.itemQueryBuilder.makeCriterias(pageNo, pageSize, params);
		return this.searcher.searchWithAggs(this.searchItemProperties.getIndexName(),
				this.searchItemProperties.getIndexType(), templateName, criterias, SearchedItem.class);
	}

	private void addChosenFrontCategory(FrontCategory frontCategory, List<Chosen> chosens) {
		if (frontCategory != null)
			chosens.add(new Chosen(3, frontCategory.getId(), frontCategory.getName()));
	}

	private void addChosenAttributes(String chosenAttrs, List<Chosen> chosens) {
		if (StringUtils.hasText(chosenAttrs)) {
			List<String> attrs = Splitters.UNDERSCORE.splitToList(chosenAttrs);
			for (String attr : attrs) {
				List parts = Splitters.COLON.splitToList(attr);
				chosens.add(new Chosen(2, parts.get(0), (String) parts.get(1)));
			}
		}
	}

	private void addChosenBrands(String brandIds, List<Chosen> chosens) {
		if (StringUtils.hasText(brandIds))
			for (String brandId : Splitters.UNDERSCORE.split(brandIds)) {
				Brand brand = this.brandCacher.findBrandById(Long.valueOf(brandId));
				chosens.add(new Chosen(1, brandId, brand.getName()));
			}
	}

	private List<AggNav> makeCategoryNavs(List<Bucket> catAggs, Set<String> chosenCategories) {
		if (CollectionUtils.isEmpty(catAggs)) {
			return null;
		}

		List catNavs = Lists.newArrayListWithCapacity(catAggs.size());
		for (Bucket catAgg : catAggs) {
			String key = catAgg.getKey();
			if (chosenCategories.contains(key))
				continue;
			try {
				Long categoryId = Long.valueOf(key);
				BackCategory backCategory = this.backCategoryCacher.findBackCategoryById(categoryId);
				if (!backCategory.getHasChildren().booleanValue())
					;
				AggNav aggNav = new AggNav(categoryId, backCategory.getName(), catAgg.getDoc_count());
				catNavs.add(aggNav);
			} catch (Exception e) {
				log.error("failed to build cat navs for bucket(key={}),cause:{}", key,
						Throwables.getStackTraceAsString(e));
			}

		}

		return catNavs;
	}

	private List<GroupedAggNav> makeGroupedAttrNavs(List<Bucket> attrAggs, String chosenAttrs) {
		if (CollectionUtils.isEmpty(attrAggs)) {
			return null;
		}

		List attrNavs = Lists.newArrayListWithCapacity(attrAggs.size());

		Set chosenKeys = Sets.newHashSet();
		if (StringUtils.hasText(chosenAttrs)) {
			List<String> attrs = Splitters.UNDERSCORE.splitToList(chosenAttrs);
			for (String attr : attrs) {
				List parts = Splitters.COLON.splitToList(attr);
				chosenKeys.add(parts.get(0));
			}
		}

		for (Bucket attrAgg : attrAggs) {
			List keyAndVal = Splitters.COLON.splitToList(attrAgg.getKey());

			if (!chosenKeys.contains(keyAndVal.get(0))) {
				AggNav aggNav = new AggNav(keyAndVal.get(0), (String) keyAndVal.get(1), attrAgg.getDoc_count());
				attrNavs.add(aggNav);
			}

		}

		Multimap byKey = Multimaps.index(attrNavs, new Function<AggNav, Object>() {
			public Object apply(AggNav attrNav) {
				return attrNav.getKey();
			}
		});
		List result = Lists.newArrayListWithCapacity(10);
		for (Iterator i$ = Iterables.limit(byKey.keySet(), 10).iterator(); i$.hasNext();) {
			Object key = i$.next();
			Iterable<AggNav> aggNavs = byKey.get(key);
			String group = String.valueOf(key);

			List nameAndCounts = Lists.newArrayListWithCapacity(20);
			for (AggNav aggNav : Iterables.limit(aggNavs, 20)) {
				nameAndCounts.add(new NameAndCount(aggNav.getName(), Integer.valueOf(aggNav.getCount().intValue())));
			}
			result.add(new GroupedAggNav(group, nameAndCounts));
		}

		return result;
	}

	private List<AggNav> makeBrandNavs(List<Bucket> brandAggs) {
		if (CollectionUtils.isEmpty(brandAggs)) {
			return null;
		}
		List brandNavs = Lists.newArrayListWithCapacity(brandAggs.size());
		for (Bucket brandAgg : brandAggs) {
			try {
				Long brandId = Long.valueOf(brandAgg.getKey());
				Brand brand = this.brandCacher.findBrandById(brandId);
				AggNav aggNav = new AggNav(brandId, brand.getName(), brandAgg.getDoc_count());

				Map extra = Collections.singletonMap("img", brand.getLogo());
				aggNav.setExtra(extra);
				brandNavs.add(aggNav);
			} catch (Exception e) {
				log.error("failed to build brand navs for bucket(key={}),cause:{}", brandAgg.getKey(),
						Throwables.getStackTraceAsString(e));
			}
		}

		return brandNavs;
	}

	private List<AggNav> makeShopCategoryNavs(List<Bucket> shopCategoryAggs, String chosenShopCatId) {
		if (CollectionUtils.isEmpty(shopCategoryAggs)) {
			return null;
		}

		List shopCategoryNavs = Lists.newArrayListWithCapacity(shopCategoryAggs.size());
		Long chosenShopCategoryId;
		if (StringUtils.hasText(chosenShopCatId)) {
			chosenShopCategoryId = Long.valueOf(chosenShopCatId);
			if (Objects.equal(chosenShopCategoryId, Long.valueOf(-1L))) {
				return null;
			}

			for (Bucket shopCategoryAgg : shopCategoryAggs) {
				Long shopCategoryId = Long.valueOf(shopCategoryAgg.getKey());

				if (Objects.equal(shopCategoryId, chosenShopCategoryId)) {
					continue;
				}

				ShopCategory shopCategory = this.shopCategoryCacher.findById(shopCategoryId);
				if (Objects.equal(shopCategory.getPid(), chosenShopCategoryId)) {
					AggNav aggNavOfNextLevel = new AggNav(shopCategoryId, shopCategory.getName(),
							shopCategoryAgg.getDoc_count());
					shopCategoryNavs.add(aggNavOfNextLevel);
				}
			}
		} else {
			for (Bucket shopCategoryAgg : shopCategoryAggs) {
				Long shopCategoryId = Long.valueOf(shopCategoryAgg.getKey());

				if (Objects.equal(shopCategoryId, Long.valueOf(-1L))) {
					AggNav aggNavOfUnknown = new AggNav(Long.valueOf(-1L), "未分类", shopCategoryAgg.getDoc_count());
					shopCategoryNavs.add(aggNavOfUnknown);
				} else {
					ShopCategory shopCategory = this.shopCategoryCacher.findById(shopCategoryId);
					if (Objects.equal(shopCategory.getPid(), Long.valueOf(0L))) {
						AggNav aggNavOfFirstLevel = new AggNav(shopCategoryId, shopCategory.getName(),
								shopCategoryAgg.getDoc_count());
						shopCategoryNavs.add(aggNavOfFirstLevel);
					}
				}
			}
		}
		return shopCategoryNavs;
	}

	private List<IdAndName> makeBreadCrumbs(Long currentCategoryId) {
		List idAndNames = Lists.newArrayList();
		while (currentCategoryId.longValue() > 0L) {
			BackCategory backCategory = this.backCategoryCacher.findBackCategoryById(currentCategoryId);
			idAndNames.add(new IdAndName(backCategory.getId(), backCategory.getName()));
			currentCategoryId = backCategory.getPid();
		}
		List breadCrumbs = Lists.newArrayListWithCapacity(idAndNames.size() + 1);
		breadCrumbs.add(new IdAndName(Long.valueOf(0L), "所有分类"));
		breadCrumbs.addAll(Lists.reverse(idAndNames));
		return breadCrumbs;
	}

	private List<IdAndName> makeBreadCrumbsOfShopCategory(Long currentShopCategoryId) {
		List idAndNames = Lists.newArrayList();
		while (currentShopCategoryId.longValue() > 0L) {
			ShopCategory shopCategory = this.shopCategoryCacher.findById(currentShopCategoryId);
			idAndNames.add(new IdAndName(shopCategory.getId(), shopCategory.getName()));
			currentShopCategoryId = shopCategory.getPid();
		}
		List breadCrumbs = Lists.newArrayListWithCapacity(idAndNames.size() + 1);
		breadCrumbs.add(new IdAndName(Long.valueOf(0L), "所有分类"));
		breadCrumbs.addAll(Lists.reverse(idAndNames));
		return breadCrumbs;
	}

	private void leafOf(FrontCategoryTree frontCategoryTree, List<Long> leafFcids) {
		if (CollectionUtils.isEmpty(frontCategoryTree.getChildren())) {
			leafFcids.add(frontCategoryTree.getCurrent().getId());
		} else
			for (FrontCategoryTree categoryTree : frontCategoryTree.getChildren())
				leafOf(categoryTree, leafFcids);
	}
}