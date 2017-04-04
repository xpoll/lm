package cn.blmdz.aide.pay.channel.kjtpay.util.itrus;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Strings;
import com.itrus.cert.X509Certificate;
import com.itrus.cryptorole.bc.RecipientBcImpl;
import com.itrus.cryptorole.bc.SenderBcImpl;
import com.itrus.cvm.CVM;
import com.itrus.svm.SignerAndEncryptedDigest;
import com.itrus.util.Base64;

import cn.blmdz.aide.pay.channel.kjtpay.pojo.VerifyResult;

public class ITrus implements Signer {
	private static Logger logger = LoggerFactory.getLogger(ITrus.class);
	private String cvmConfigFile = "";
	private String pfxFileName = "";
	private String keyPassword = "";
	private String certFileName = "";
	private SenderBcImpl sender = new SenderBcImpl();

	public void init() {
		try {
			if (!Strings.isNullOrEmpty(this.cvmConfigFile)) {
				CVM.config(this.cvmConfigFile);
			} else {
				logger.info("未配置CVM配置文件，CVM将不会初始化");
			}

			if (!Strings.isNullOrEmpty(this.pfxFileName)) {
				this.sender.initCertWithKey(this.pfxFileName, this.keyPassword);
			} else {
				logger.info("未配置私钥证书，加签服务将不会初始化");
			}
		} catch (Exception var2) {
			logger.error(var2.getMessage());
		}

	}

	@Override
	public String sign(String oriText, String privateKey, String charset) throws Exception {
		return this.sender.signMessage(oriText);
	}

	public String getCvmConfigFile() {
		return this.cvmConfigFile;
	}

	public void setCvmConfigFile(String cvmConfigFile) {
		this.cvmConfigFile = cvmConfigFile;
	}

	public String getPfxFileName() {
		return this.pfxFileName;
	}

	public void setPfxFileName(String pfxFileName) {
		this.pfxFileName = pfxFileName;
	}

	public String getKeyPassword() {
		return this.keyPassword;
	}

	public void setKeyPassword(String keyPassword) {
		this.keyPassword = keyPassword;
	}

	public String getCertFileName() {
		return this.certFileName;
	}

	public void setCertFileName(String certFileName) {
		this.certFileName = certFileName;
	}

	@Override
	public VerifyResult verify(String oriText, String sign, String publicKey, String charset) throws Exception {
		RecipientBcImpl recipient = new RecipientBcImpl();
		SignerAndEncryptedDigest ret = null;
		byte[] toSignBuf = oriText.getBytes(charset);
		VerifyResult result = null;

		try {
			ret = recipient.verifyAndParsePkcs7(toSignBuf, Base64.decode(sign));
			X509Certificate cert = X509Certificate.getInstance(ret.getSigner());
			result = new VerifyResult(true);
			result.addInfo("subjectDN", cert.getSubjectDNString());
			int cvm = CVM.verifyCertificate(cert);
			logger.info("证书验证结果，Return=[" + cvm + "]，");
			if (cvm != 0) {
				String cvmMsg = "";
				switch (cvm) {
				case -1:
					cvmMsg = "CVM初始化错误，请检查配置文件或给CVM增加支持的CA。";
				case 0:
				default:
					break;
				case 1:
					cvmMsg = "证书已过期。";
					break;
				case 2:
					cvmMsg = "证书已吊销。";
					break;
				case 3:
					cvmMsg = "不支持的颁发者。请检查cvm.xml配置文件";
					break;
				case 4:
					cvmMsg = "非法颁发者。";
					break;
				case 5:
					cvmMsg = "CRL不可用，未知状态。";
					break;
				case 6:
					cvmMsg = "证书被吊销且已过期。";
				}

				logger.info("证书非法:" + cvmMsg);
				result.setSuccess(false);
				result.addInfo("exMsg", cvmMsg);
			}
		} catch (Exception var12) {
			logger.error(var12.getMessage());
			result = new VerifyResult(false);
			result.addInfo("exMsg", var12.getMessage());
		}

		return result;
	}
}
