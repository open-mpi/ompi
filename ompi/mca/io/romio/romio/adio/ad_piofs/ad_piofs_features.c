int ADIOI_PIOFS_Features(int flag)
{
	switch(flag) {
		case ADIO_LOCKS:
		case ADIO_SHARED_FP:
		case ADIO_ATOMIC_MODE:
		case ADIO_DATA_SIEVING_WRITES:
		case ADIO_SCALABLE_OPEN:
		default:
			return 0;
			break;
	}
}
